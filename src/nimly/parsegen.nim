import macros
import tables
import sets

import patty

import parsetypes
import parser

type
  PTProc[T, S, R] = proc(nimlytree: ParseTree[T, S]): R {.nimcall.}
  RuleToProc*[T, S, R] = Table[Rule[S], PTProc[T, S, R]]
  NimyKind = enum
    NonTerm
    Term
  NimyRow = object
    kind: NimyKind
    retTyNode: NimNode
    ruleToProc: NimNode
    optRule: NimNode
    repRule: NimNode
  NimyInfo = Table[string, NimyRow]

iterator `&`(a, b: NimNode): (int, NimNode) =
  yield (0, a)
  for i, val in b:
    yield (i + 1, val)

proc initNimyRow(kind: NimyKind,
                 rtn: NimNode = newEmptyNode(),
                 rtp: NimNode = newEmptyNode(),
                 opr: NimNode = newEmptyNode(),
                 rpr: NimNode = newEmptyNode()): NimyRow =
  result = NimyRow(kind: kind, retTyNode: rtn, ruleToProc: rtp, optRule: opr,
                   repRule: rpr)

proc isNonTerm(nimyInfo: NimyInfo, s: string): bool =
  if not nimyInfo.haskey(s):
    return false
  return nimyInfo[s].kind == NonTerm

proc initNimyInfo(): NimyInfo =
  return initTable[string, NimyRow]()

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

proc convertToSymNode(name: string, kindTy: NimNode,
                      nimyInfo: NimyInfo): NimNode =
  if nimyInfo.isNonTerm(name):
    result = nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("NonTermS"),
        kindTy
      ),
      newStrLitNode(name)
    )
  else:
    result = nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("TermS"),
        kindTy
      ),
      genKindNode(kindTy, newIdentNode(name))
    )

proc convertToSymNode(node, kindTy: NimNode,
                      nimyInfo: NimyInfo,
                      noEmpty: bool = true): NimNode =
  node.expectKind({nnkIdent, nnkBracket})
  if node.kind == nnkBracket:
    doAssert node.len == 0 and (not (noEmpty)), "rule cannot empty or" &
      " contains [] if the rule is not empty"
    return nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("Empty"),
        kindTy
      )
    )
  else:
    let name = $(node.ident)
    return convertToSymNode(name, kindTy, nimyInfo)

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

proc nonTermOrEmpty(node: NimNode, nimyInfo: NimyInfo): string =
  node.expectKind({nnkBracket, nnkIdent})
  if node.kind == nnkBracket:
    return ""
  let s = $(node.ident)
  if nimyInfo.isNonTerm(s):
    result = s
  else:
    result = ""

proc isTerm(node: NimNode, nimyInfo: NimyInfo): bool =
  node.expectKind({nnkBracket, nnkIdent})
  if node.kind == nnkBracket:
    return false
  elif not (nimyInfo.isNonTerm($(node.ident))):
    return true
  return false

proc parseRuleAndBody(node, kindTy, tokenType, left: NimNode,
                      nimyInfo: var NimyInfo): (
                        NimNode, seq[string], NimNode) =
  node.expectKind({nnkCall, nnkCommand})
  var types: seq[string] = @[]
  case node.kind:
  of nnkCall:
    let
      rightNode = node[0].convertToSymNode(kindTy, nimyInfo, noEmpty = false)
      ruleMaker = newRuleMakerNode(kindTy, left, rightNode)
    if node[0].isTerm(nimyInfo) and not(nimyInfo.haskey($(node[0].ident))):
      nimyInfo[$(node[0].ident)] = initNimyRow(Term)
    types.add(node[0].nonTermOrEmpty(nimyInfo))
    result = (ruleMaker, types, node[1])
  of nnkCommand:
    var
      right: seq[NimNode] = @[]
      cmd: NimNode = node
    while cmd.kind == nnkCommand:
      if cmd[0].isTerm(nimyInfo) and not(nimyInfo.haskey($(cmd[0].ident))):
        nimyInfo[$(cmd[0].ident)] = initNimyRow(Term)
      right.add(cmd[0].convertToSymNode(kindTy, nimyInfo))
      types.add(cmd[0].nonTermOrEmpty(nimyInfo))
      cmd = cmd[1]
    if cmd.isTerm(nimyInfo) and not(nimyInfo.haskey($(cmd.ident))):
      nimyInfo[$(cmd.ident)] = initNimyRow(Term)
    right.add(cmd.convertToSymNode(kindTy, nimyInfo))
    types.add(cmd.nonTermOrEmpty(nimyInfo))

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
            nimyInfo[types[index]].ruleToProc,
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
    param = newIdentNode("nimlytree")
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

proc addVarSymToInt(stm : var NimNode,
                    id, tokenKind: NimNode, syms: seq[NimNode]) =
  stm.expectKind(nnkStmtList)
  stm.add(
    newVarStmt(
      id,
      nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("initTable"),
          nnkBracketExpr.newTree(
            newIdentNode("Symbol"),
            tokenKind
          ),
          newIdentNode("int")
        )
      )
    )
  )
  for i, sym in syms:
    stm.add(
      nnkAsgn.newTree(
        nnkBracketExpr.newTree(
          id,
          sym
        ),
        newIntLitNode(i)
      )
    )

proc addRuleToInt(stm : var NimNode,
                  id, tokenKind: NimNode, rules: seq[NimNode]) =
  stm.expectKind(nnkStmtList)
  stm.add(
    newVarStmt(
      id,
      nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("initTable"),
          nnkBracketExpr.newTree(
            newIdentNode("Rule"),
            tokenKind
          ),
          newIdentNode("int")
        )
      )
    )
  )
  for i, r in rules:
    stm.add(
      nnkAsgn.newTree(
        nnkBracketExpr.newTree(
          id,
          r
        ),
        newIntLitNode(- (i + 1))
      )
    )

proc addIntToRule(stm : var NimNode,
                  id, tokenKind: NimNode, rules: seq[NimNode]) =
  stm.expectKind(nnkStmtList)
  stm.add(
    newVarStmt(
      id,
      nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("initTable"),
          newIdentNode("int"),
          nnkBracketExpr.newTree(
            newIdentNode("Rule"),
            tokenKind
          )
        )
      )
    )
  )
  for i, r in rules:
    stm.add(
      nnkAsgn.newTree(
        nnkBracketExpr.newTree(
          id,
        newIntLitNode(- (i + 1))
        ),
        r
      )
    )

proc addVarIntToSym(stm : var NimNode,
                    id, tokenKind: NimNode, syms: seq[NimNode]) =
  stm.expectKind(nnkStmtList)
  stm.add(
    newVarStmt(
      id,
      nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("initTable"),
          newIdentNode("int"),
          nnkBracketExpr.newTree(
            newIdentNode("Symbol"),
            tokenKind
          )
        )
      )
    )
  )
  for i, sym in syms:
    stm.add(
      nnkAsgn.newTree(
        nnkBracketExpr.newTree(
          id,
          newIntLitNode(i)
        ),
        sym
      )
    )

proc tableMakerProc(name, tokenType, tokenKind, topNonTerm, tableMaker: NimNode,
                    rules, ruleDefs, syms: seq[NimNode]): NimNode =
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
        nnkBracketExpr.newTree(
          newIdentNode("seq"),
          nnkBracketExpr.newTree(
            newIdentNode("Rule"),
            tokenKind
          )
        ),
        nnkPrefix.newTree(
          newIdentNode("@"),
          nnkBracket.newTree(
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
          newIdentNode("add")
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
  let tmpTable = genSym()
  body.add(
    newLetStmt(
      tmpTable,
      nnkCall.newTree(
        nnkBracketExpr.newTree(
          tableMaker,
          tokenKind
        ),
        grmId
      )
    )
  )
  let stiId = genSym(nskVar)
  body.addVarSymToInt(stiId, tokenKind, syms)
  let rtiId = genSym(nskVar)
  body.addRuleToInt(rtiId, tokenKind, rules)
  body.add(
    nnkAsgn.newTree(
      newIdentNode("result"),
      nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("toConst"),
          tokenKind
        ),
        tmpTable,
        stiId,
        rtiId
      )
    )
  )
  result = newProc(
    name,
    @[newIdentNode("ConstTable")],
    body
  )

macro nimy*(head, body: untyped): untyped =
  head.expectKind(nnkBracketExpr)
  body.expectKind(nnkStmtList)
  var
    tableMaker = newIdentNode("makeTableLALR")
  let
    parserName = head[0]
    tokenType = head[1]
    tokenKind = parseStmt($tokenType.ident & "Kind")[0]
  for i, hd in head:
    if i > 1:
      if hd.kind == nnkIdent and $hd == "LR0":
        tableMaker = newIdentNode("makeTableLR")
  var
    nimyInfo = initNimyInfo()
    first = true
    topNonTerm: string
    topNonTermNode: NimNode
    returnType: Nimnode
    ruleIds: seq[NimNode] = @[]
    ruleDefs: seq[NimNode] = @[]
    ruleProcs: seq[NimNode] = @[]
    ruleToProcMakers: seq[NimNode] = @[]
    tableConstDefs: seq[NimNode] = @[]
    ruleProcPts: seq[NimNode] = @[]
    symNodes: seq[NimNode] = @[]
  let topProcId = genSym(nskProc)
  result = newTree(nnkStmtList)

  # read BNF first (collert info)
  for clause in body:
    if clause.kind == nnkCommentStmt:
      continue
    let (nonTerm, rType) = parseLeft(clause)
    doAssert (not (nimyInfo.haskey(nonTerm))), "some nonterm are duplicated"
    nimyInfo[nonTerm] = initNimyRow(NonTerm, rtn = rType, rtp = genSym())
    if first:
      topNonTerm = nonTerm
      topNonTermNode = nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("NonTermS"),
          tokenKind
        ),
        newStrLitNode(nonTerm)
      )
      returnType = rType
      first = false
  nimyInfo["__Start__"] = initNimyRow(NonTerm, rtn = returnType, rtp = genSym())

  # make top clause proc
  let topClause = nnkCall.newTree(
    nnkBracketExpr.newTree(
      newIdentNode("__Start__"),
      returnType
    ),
    nnkStmtList.newTree(
      nnkCall.newTree(
        newIdentNode(topNonTerm),
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("$"),
              newLit(1)
            )
          )
        )
      )
    )
  )

  #read BNF second (make procs)
  for i, clause in topClause & body:
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
          ruleClause, tokenKind, tokenType, left, nimyInfo
        )
        ruleId = genSym()
        ruleProcId = if i == 0:
                       topProcId
                     else:
                       genSym(nskProc)
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
        makeRuleProc(ruleProcId, clauseBody, nimyInfo[nonTerm].retTyNode,
                     tokenType, tokenKind, argTypes, nimyInfo)
      )
      ruleProcPts.add(
        makeRuleProc(ruleProcId, clauseBody, nimyInfo[nonTerm].retTyNode,
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
        nimyInfo[nonTerm].ruleToProc,
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
  for nt in nimyInfo.keys:
    symNodes.add(convertToSymNode(nt, tokenKind, nimyInfo))
  symNodes.add(
    newCall(
      nnkBracketExpr.newTree(
        newIdentNode("End"),
        tokenKind
      )
    )
  )
  let
    tmpName = genSym(nskProc)
  var
    tableName: NimNode
  result.add(
    tableMakerProc(tmpName, tokenType, tokenKind, topNonTermNode, tableMaker,
                   ruleIds, ruleDefs, symNodes)
  )
  var constTableName: NimNode
  when defined(nimylet):
    constTableName = genSym()
    result.add(
      newLetStmt(
        constTableName,
        nnkCall.newTree(
          tmpName
        )
      )
    )
  else:
    constTableName = genSym(nskConst)
    result.add(
      newConstStmt(
        constTableName,
        nnkCall.newTree(
          tmpName
        )
      )
    )

  let
    its = genSym(nskVar)
    itr = genSym(nskVar)
    letTable = genSym()

  result.add(ruleDefs)
  result.addVarIntToSym(its, tokenKind, symNodes)
  result.addIntToRule(itr, tokenKind, ruleIds)
  result.add(
    newLetStmt(
      letTable,
      newCall(
        nnkBracketExpr.newTree(
          newIdentNode("reconstruct"),
          tokenKind
        ),
        constTableName,
        its,
        itr
      )
    )
  )

  result.add(
    newVarStmt(
      nnkPostfix.newTree(
        newIdentNode("*"),
        parserName
      ),
      nnkCall.newTree(
        nnkBracketExpr.newTree(
          newIdentNode("newParser"),
          tokenKind
        ),
        letTable
      )
    )
  )

  # add proc parse
  result.add(
    nnkProcDef.newTree(
      nnkPostfix.newTree(
        newIdentNode("*"),
        newIdentNode("parse")
      ),
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
            newIdentNode("tree"),
          )
        )
      )
    )
  )
  when defined(nimydebug):
    echo toStrLit(result)
