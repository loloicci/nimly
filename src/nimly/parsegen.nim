import macros
import tables
import sets

import patty

import lalr

type
  PTProc[T, S, R] = proc(tree: ParseTree[T, S]): R {.nimcall.}
  RuleToProc*[T, S, R] = Table[Rule[S], PTProc[T, S, R]]
  # nontermStr -> (retTyNode, ruleToProc id, (clauseNo -> procName))
  NimyInfo = Table[string, (NimNode, NimNode)]

proc initNimyInfo(): NimyInfo =
  return initTable[string, (NimNode, NimNode)]()

proc initRuleToProc*[T, S, R](): RuleToProc[T, S, R] =
  return initTable[Rule[S], PTProc[T, S, R]]()

proc initRuleToProcNode(tokenType, tokenKind, returnType: NimNode): NimNode =
  result = nnkAsgn.newTree(
    newIdentNode("result"),
    nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("initRuleToProc"),
        tokenType,
        tokenKind,
        returnType
      )
    )
  )

proc genKindNode(kindTy, kind: NimNode): NimNode =
  result = nnkDotExpr.newTree(
    kindTy,
    kind
  )

proc convertToSymNode(node, kindTy: NimNode,
                      nonTerms: HashSet[string]): NimNode =
  node.expectKind(nnkIdent)
  let sym = $(node.ident)
  if sym in nonTerms:
    result = nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("NonTermS"),
        kindTy
      ),
      newStrLitNode(sym)
    )
  else:
    result = nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("TermS"),
        kindTy
      ),
      genKindNode(kindTy, node)
    )

proc newRuleMakerNode(kindTy, left: NimNode,
                      right: varargs[NimNode]): NimNode =
  result = nnkCall.newTree(
    nnkBracketExpr.newTree(
      newIdentNode("newRule"),
      kindTy
    ),
    left
  )
  for node in right:
    result.add(node)

proc getTypeNode(n, tokenType: NimNode,
                 nonTerms: HashSet[string], nimyInfo: NimyInfo): NimNode =
  let sym = $(n.ident)
  if sym in nonTerms:
    result = nimyInfo[sym][0]
  else:
    result = tokenType

proc nonTermOrEmpty(node: NimNode, nonTerms: HashSet[string]): string =
  let s = $(node.ident)
  if s in nonTerms:
    result = s
  else:
    result = ""

proc parseRuleAndBody(node, kindTy, tokenType, left: NimNode,
                      nonTerms: HashSet[string], nimyInfo: NimyInfo): (
                        NimNode, seq[string], NimNode) =
  node.expectKind({nnkCall, nnkCommand})
  var types: seq[string] = @[]
  case node.kind:
  of nnkCall:
    let
      rightNode = node[0].convertToSymNode(kindTy, nonTerms)
      ruleMaker = newRuleMakerNode(kindTy, left, rightNode)
    # types.add(getTypeNode(node[0], nonTerms, nimyInfo))
    types.add(node[0].nonTermOrEmpty(nonTerms))
    result = (ruleMaker, types, node[1])
  of nnkCommand:
    var
      right: seq[NimNode] = @[]
      cmd: NimNode = node
    while cmd.kind == nnkCommand:
      right.add(cmd[0].convertToSymNode(kindTy, nonTerms))
      # types.add(cmd[0].getTypeNode(nonTerms, nimyInfo))
      types.add(cmd[0].nonTermOrEmpty(nonTerms))
      cmd = cmd[1]
    right.add(cmd.convertToSymNode(kindTy, nonTerms))
    # types.add(cmd.getTypeNode(nonTerms, nimyInfo))
    types.add(cmd.nonTermOrEmpty(nonTerms))

    let ruleMaker = newRuleMakerNode(kindTy, left, right)
    result = (ruleMaker, types, node[2])
  else:
    doAssert false

proc parseLeft(clause: NimNode): (string, NimNode) =
  clause.expectKind(nnkCall)
  clause[0].expectKind(nnkBracketExpr)
  doAssert clause[0].len == 2
  let
    nonTerm = $(clause[0][0].ident)
    rType = clause[0][1]
  return (nonTerm, rType)

proc isSpecialVar(n: NimNode): bool =
  return (n.kind == nnkPrefix and
          n.len == 2 and
          n[0] == newIdentNode("$") and
          n[1].kind == nnkIntLit)

proc replaceBody(body, param: NimNode,
                 types: seq[string], nimyInfo: NimyInfo): NimNode =
  proc replaceImpl(body: NimNode): NimNode =
    if body.isSpecialVar:
      let index = int((body[1].intVal) - 1)
      # term
      if types[index] == "":
        return nnkDotExpr.newTree(
          nnkBracketExpr.newTree(
            nnkDotExpr.newTree(
              param,
              newIdentNode("tree")
            ),
            newIntLitNode(index)
          ),
          newIdentNode("token")
        )
      # nonterm
      else:
        # table[param[index].rule](param[index].tree)
        return nnkCall.newTree(
          nnkBracketExpr.newTree(
            nimyInfo[types[index]][1],
            nnkDotExpr.newTree(
              nnkBracketExpr.newTree(
                nnkDotExpr.newTree(
                  param,
                  newIdentNode("tree")
                ),
                newIntLitNode(index)
              ),
              newIdentNode("rule")
            )
          ),
          nnkBracketExpr.newTree(
            nnkDotExpr.newTree(
              param,
              newIdentNode("tree")
            ),
            newIntLitNode(index)
          ),
        )

    else:
      if body.len > 0:
        result = newTree(body.kind)
        for c in body:
          result.add(c.replaceImpl)
      else:
        result = body
  result = replaceImpl(body)

proc makeRuleProc(name, body, rTy, tokenType, tokenKind: NimNode,
                  types: seq[string], nimyInfo: NimyInfo, pt=false): NimNode =
  let
    param = newIdentNode("tree")
    pTy =   nnkBracketExpr.newTree(newIdentNode("ParseTree"),
                                   tokenType, tokenKind)
    params = @[rTy, nnkIdentDefs.newTree(param, pTy, newEmptyNode())]
  var
    procBody: NimNode
  if not pt:
    procBody = body.replaceBody(param, types, nimyInfo)
    result = newProc(name, params, procBody)
  else:
    result = newProc(name, params)

proc tableMakerProc(name, tokenType, tokenKind, topNonTerm: NimNode,
                    rules, ruleDefs: seq[NimNode]): NimNode =
  var body = nnkStmtList.newTree()
  for rd in ruleDefs:
    body.add(rd)
  let
    setId = genSym(nskVar)
    grmId = genSym()
  body.add(
    nnkVarSection.newTree(
      nnkIdentDefs.newTree(
        setId,
        newEmptyNode(),
        nnkCall.newTree(
          nnkBracketExpr.newTree(
            newIdentNode("initSet"),
            nnkBracketExpr.newTree(
              newIdentNode("Rule"),
              tokenKind
            )
          )
        )
      )
    )
  )
  for rule in rules:
    body.add(
      nnkCall.newTree(
        nnkDotExpr.newTree(
          setId,
          newIdentNode("incl")
        ),
        rule
      )
    )
  body.add(
    newLetStmt(
      grmId,
      nnkCall.newTree(
        newIdentNode("initGrammar"),
        setId,
        topNonTerm
      )
    )
  )
  body.add(
    nnkAsgn.newTree(
      newIdentNode("result"),
      nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("makeTable"),
          tokenKind
        ),
        grmId
      )
    )
  )
  result = newProc(
    name,
    @[nnkBracketExpr.newTree(
      newIdentNode("ParsingTable"),
      tokenKind
    )],
    body
  )

macro nimy*(head, body: untyped): untyped =
  head.expectKind(nnkBracketExpr)
  body.expectKind(nnkStmtList)
  let
    parserName = head[0]
    tokenType = head[1]
    tokenKind = parseStmt($tokenType.ident & "Kind")[0]
  var
    nimyInfo = initNimyInfo()
    nonTerms = initSet[string]()
    first = true
    topNonTermNode: NimNode
    topProcId: NimNode
    returnType: Nimnode
    ruleIds: seq[NimNode] = @[]
    ruleDefs: seq[NimNode] = @[]
    ruleProcs: seq[NimNode] = @[]
    ruleToProcMakers: seq[NimNode] = @[]
    tableConstDefs: seq[NimNode] = @[]
    ruleProcPts: seq[NimNode] = @[]
  result = newTree(nnkStmtList)

  # read BNF first (collert info)
  for clause in body:
    if clause.kind == nnkCommentStmt:
      continue
    let (nonTerm, rType) = parseLeft(clause)
    doAssert (not (nonTerm in nonTerms)), "some nonterm are duplicated"
    nonTerms.incl(nonTerm)
    nimyInfo[nonTerm] = (rType, genSym())
    if first:
      topNonTermNode = nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("NonTermS"),
          tokenKind
        ),
        newStrLitNode(nonTerm)
      )
      returnType = rType
      first = false

  first = true
  #read BNF second (make procs)
  for i, clause in body:
    if clause.kind == nnkCommentStmt:
      continue
    let
      (nonTerm, rType) = parseLeft(clause)
      ruleClauses = clause[1]
    var ruleToProcMakerBody = nnkStmtList.newTree(
      initRuleToProcNode(tokenType, tokenKind, rType)
    )

    # read Rule
    for j, ruleClause in ruleClauses:
      if ruleClause.kind == nnkCommentStmt:
        continue
      let
        left = nnkCall.newTree(
          nnkBracketExpr.newTree(
            newIdentNode("NonTermS"),
            tokenKind
          ),
          newStrLitNode(nonTerm)
        )
        # argTypes: seq[bool] (true if nonterm)
        (ruleMaker, argTypes, clauseBody) = parseRuleAndBody(
          ruleClause, tokenKind, tokenType, left,
          nonTerms, nimyInfo
        )
        ruleId = genSym()
        ruleProcId = genSym(nskProc)
      if first:
        topProcId = ruleProcId
        first = false
      ruleIds.add(ruleId)
      let ruleDef = newLetStmt(
        ruleId,
        ruleMaker
      )
      # makeRule
      ruleDefs.add(
        ruleDef
      )

      # make proc and add to result
      ruleProcs.add(
        makeRuleProc(ruleProcId, clauseBody, nimyInfo[nonTerm][0],
                     tokenType, tokenKind, argTypes, nimyInfo)
      )
      ruleProcPts.add(
        makeRuleProc(ruleProcId, clauseBody, nimyInfo[nonTerm][0],
                     tokenType, tokenKind, argTypes, nimyInfo, true)
      )

      # add proc id table maker
      ruleToProcMakerBody.add(
        ruleDef
      )
      ruleToProcMakerBody.add(
        nnkAsgn.newTree(
          nnkBracketExpr.newTree(
            newIdentNode("result"),
            ruleId
          ),
          ruleProcId
        )
      )
    # ruleToProcMakerDef
    let ruleToProcMakerName = genSym(nskProc)
    var ruleToProcMakerNode = newProc(
      ruleToProcMakerName,
      @[nnkBracketExpr.newTree(
        newIdentNode("RuleToProc"),
        tokenType,
        tokenKind,
        rType
      )],
      ruleToProcMakerBody
    )
    ruleToProcMakers.add(
      ruleToProcMakerNode
    )
    # add table to result
    tableConstDefs.add(
      newLetStmt(
        nimyInfo[nonTerm][1],
        nnkCall.newTree(
          ruleToProcMakerName
        )
      )
    )
  result.add(ruleProcPts)
  result.add(ruleToProcMakers)
  result.add(tableConstDefs)
  result.add(ruleProcs)
  # makeGrammarAndParsingTable
  let
    tmpName = genSym(nskProc)
  var
    tableName: NimNode
  result.add(
    tableMakerProc(tmpName, tokenType, tokenKind, topNonTermNode, ruleIds,
                   ruleDefs)
  )
  when defined(nimydebug):
    tableName = genSym()
    result.add(
      newLetStmt(
        tableName,
        nnkCall.newTree(
          tmpName
        )
      )
    )
  else:
    tableName = genSym(nskConst)
    result.add(
      newConstStmt(
        tableName,
        nnkCall.newTree(
          tmpName
        )
      )
    )

  result.add(
    newVarStmt(
      parserName,
      nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("newParser"),
          tokenKind
        ),
        tableName
      )
    )
  )

  # add proc parse
  result.add(
    nnkProcDef.newTree(
      newIdentNode("parse"),
      newEmptyNode(),
      nnkGenericParams.newTree(
        nnkIdentDefs.newTree(
          newIdentNode("T"),
          newIdentNode("S"),
          newEmptyNode(),
          newEmptyNode()
        )
      ),
      nnkFormalParams.newTree(
        returnType,
        nnkIdentDefs.newTree(
          newIdentNode("parser"),
          nnkVarTy.newTree(
            nnkBracketExpr.newTree(
              newIdentNode("Parser"),
              newIdentNode("S")
            )
          ),
          newEmptyNode()
        ),
        nnkIdentDefs.newTree(
          newIdentNode("lexer"),
          nnkVarTy.newTree(
            nnkBracketExpr.newTree(
              newIdentNode("NimlLexer"),
              newIdentNode("T")
            )
          ),
          newEmptyNode()
        )
      ),
      newEmptyNode(),
      newEmptyNode(),
      nnkStmtList.newTree(
        nnkLetSection.newTree(
          nnkIdentDefs.newTree(
            newIdentNode("tree"),
            newEmptyNode(),
            nnkCall.newTree(
              newIdentNode("parseImpl"),
              newIdentNode("parser"),
              newIdentNode("lexer")
            )
          )
        ),
        nnkReturnStmt.newTree(
          nnkCall.newTree(
            topProcId,
            newIdentNode("tree")
          )
        )
      )
    )
  )
  when defined(nimydebug):
    echo toStrLit(result)
