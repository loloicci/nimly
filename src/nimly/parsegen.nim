import macros
import tables
import sets

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

iterator iter(a, b: NimNode, c: seq[NimNode]): (int, NimNode) =
  var cnt = 0
  yield (cnt, a)
  inc(cnt)
  for val in b:
    yield (cnt, val)
    inc(cnt)
  for val in c:
    yield (cnt, val)
    inc(cnt)

proc initNimyRow(kind: NimyKind,
                 rtn: NimNode = newEmptyNode(),
                 rtp: NimNode = newEmptyNode(),
                 opr: NimNode = newEmptyNode(),
                 rpr: NimNode = newEmptyNode()): NimyRow =
  result = NimyRow(kind: kind, retTyNode: rtn, ruleToProc: rtp, optRule: opr,
                   repRule: rpr)

proc isNonTerm(s: string, nimyInfo: NimyInfo): bool =
  if not nimyInfo.haskey(s):
    return false
  return nimyInfo[s].kind == NonTerm

proc isTerm(s: string, nimyInfo: NimyInfo): bool =
  if not nimyInfo.haskey(s):
    return false
  return  nimyInfo[s].kind == Term

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
  if name.isNonTerm(nimyInfo):
    result = nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("NonTermS"),
        kindTy
      ),
      newStrLitNode(name)
    )
  elif name.isTerm(nimyInfo):
    result = nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("TermS"),
        kindTy
      ),
      genKindNode(kindTy, newIdentNode(name))
    )
  else:
    doAssert false

proc convertToSymNode(node, kindTy: NimNode,
                      nimyInfo: NimyInfo,
                      noEmpty: bool = true): NimNode =
  node.expectKind({nnkIdent, nnkBracket, nnkBracketExpr, nnkCurlyExpr})
  case node.kind
  of nnkBracketExpr:
    doAssert node.len == 1
    let innerSym = node[0].strVal
    return nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("NonTermS"),
        kindTy
      ),
      newStrLitNode(nimyInfo[innerSym].optRule.strVal)
    )
  of nnkCurlyExpr:
    doAssert node.len == 1
    let innerSym = node[0].strVal
    return nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("NonTermS"),
        kindTy
      ),
      newStrLitNode(nimyInfo[innerSym].repRule.strVal)
    )
  of nnkBracket:
    doAssert node.len == 0 and (not (noEmpty)), "rule cannot empty or" &
      " contains [] if the rule is not empty"
    return nnkCall.newTree(
      nnkBracketExpr.newTree(
        newIdentNode("Empty"),
        kindTy
      )
    )
  of nnkIdent:
    let name = node.strVal
    return convertToSymNode(name, kindTy, nimyInfo)
  else:
    doAssert false

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
  node.expectKind({nnkBracket, nnkIdent, nnkBracketExpr, nnkCurlyExpr})
  case node.kind
  of nnkBracket:
    return ""
  of nnkBracketExpr:
    assert node.len == 1
    return nimyInfo[node[0].strVal].optRule.strVal
  of nnkCurlyExpr:
    assert node.len == 1
    return nimyInfo[node[0].strVal].repRule.strVal
  else:
    let s = node.strVal
    if s.isNonTerm(nimyInfo):
      result = s
    else:
      result = ""

proc isTerm(node: NimNode, nimyInfo: NimyInfo): bool =
  node.expectKind({nnkBracket, nnkIdent, nnkBracketExpr, nnkCurlyExpr})
  if node.kind in {nnkBracket, nnkBracketExpr, nnkCurlyExpr}:
    return false
  elif not (node.strVal.isNonTerm(nimyInfo)):
    return true
  return false

iterator ruleRight(node: NimNode): NimNode =
  case node.kind
  of nnkCall:
    yield node[0]
  of nnkCommand:
    var nd = node
    while nd.kind == nnkCommand:
      yield nd[0]
      nd = nd[1]
    yield nd
  else:
    assert false

proc parseRuleAndBody(node, kindTy, tokenType, left: NimNode,
                      nimyInfo: var NimyInfo): (
                        NimNode, seq[string], NimNode) =
  node.expectKind({nnkCall, nnkCommand})
  var
    right: seq[NimNode] = @[]
    types: seq[string] = @[]
    body: NimNode
    noEmpty: bool

  case node.kind:
  of nnkCall:
    body = node[1]
    noEmpty = false
  of nnkCommand:
    body = node[2]
    noEmpty = true
  else:
    doAssert false

  for sym in node.ruleRight:
    right.add(sym.convertToSymNode(kindTy, nimyInfo, noEmpty))
    types.add(sym.nonTermOrEmpty(nimyInfo))
  let ruleMaker = newRuleMakerNode(kindTy, left, right)
  result = (ruleMaker, types, body)

proc parseLeft(clause: NimNode): (string, NimNode) =
  clause.expectKind(nnkCall)
  clause[0].expectKind(nnkBracketExpr)
  doAssert clause[0].len == 2
  let
    nonTerm = clause[0][0].strVal
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

proc tableMakerProc(name, tokenType, tokenKind, topNonTerm,
                    tableMaker: NimNode,
                    rules, ruleDefs, syms: seq[NimNode]): NimNode =
  var body = nnkStmtList.newTree()
  body.add(
    nnkWhenStmt.newTree(
      nnkElifBranch.newTree(
        nnkCall.newTree(
          newIdentNode("defined"),
          newIdentNode("nimlydebug")
        ),
        nnkStmtList.newTree(
          nnkCommand.newTree(
            newIdentNode("echo"),
            newLit("START: makeing the Parser")
          )
        )
      )
    )
  )
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
  body.add(
    nnkAsgn.newTree(
      newIdentNode("result"),
      nnkCall.newTree(
        nnkBracketExpr.newTree(
          tableMaker,
          tokenKind
        ),
        grmId
      )
    )
  )

  result = newProc(
    name,
    @[
      nnkBracketExpr.newTree(
        newIdentNode("ParsingTable"),
        tokenKind
      )
    ],
    body
  )

proc getOpt(sym, ty, nt: NimNode): NimNode =
  result = nnkCall.newTree(
    nnkBracketExpr.newTree(
      nt,
      nnkBracketExpr.newTree(
        newIdentNode("seq"),
        ty
      )
    ),
    nnkStmtList.newTree(
      nnkCall.newTree(
        sym,
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("@"),
              nnkBracket.newTree(
                nnkPrefix.newTree(
                  newIdentNode("$"),
                  newLit(1)
                )
              )
            )
          )
        )
      ),
      nnkCall.newTree(
        nnkBracket.newTree(
        ),
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("@"),
              nnkBracket.newTree(
              )
            )
          )
        )
      )
    )
  )

proc getRepOpt(sym, ty, nt: NimNode): NimNode =
  result = nnkCall.newTree(
    nnkBracketExpr.newTree(
      nt,
      nnkBracketExpr.newTree(
        newIdentNode("seq"),
        ty
      )
    ),
    nnkStmtList.newTree(
      nnkCall.newTree(
        sym,
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("$"),
              newLit(1)
            )
          )
        )
      ),
      nnkCall.newTree(
        nnkBracket.newTree(
        ),
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("@"),
              nnkBracket.newTree(
              )
            )
          )
        )
      )
    )
  )

proc getRep(sym, ty, nt, nnt: NimNode): seq[NimNode] =
  result = @[]
  result.add(getRepOpt(nnt, ty, nt))
  let new = nnkCall.newTree(
    nnkBracketExpr.newTree(
      nnt,
      nnkBracketExpr.newTree(
        newIdentNode("seq"),
        ty
      )
    ),
    nnkStmtList.newTree(
      nnkCommand.newTree(
        nnt,
        sym,
        nnkStmtList.newTree(
          nnkAsgn.newTree(
            newIdentNode("result"),
            nnkPrefix.newTree(
              newIdentNode("$"),
              newLit(1)
            )
          ),
          nnkCall.newTree(
            nnkDotExpr.newTree(
              newIdentNode("result"),
              newIdentNode("add")
            ),
            nnkPrefix.newTree(
              newIdentNode("$"),
              newLit(2)
            )
          )
        )
      ),
      nnkCall.newTree(
        sym,
        nnkStmtList.newTree(
          nnkReturnStmt.newTree(
            nnkPrefix.newTree(
              newIdentNode("@"),
              nnkBracket.newTree(
                nnkPrefix.newTree(
                  newIdentNode("$"),
                  newLit(1)
                )
              )
            )
          )
        )
      )
    )
  )
  result.add(new)

macro nimy*(head, body: untyped): untyped =
  head.expectKind(nnkBracketExpr)
  body.expectKind(nnkStmtList)
  var
    tableMaker = newIdentNode("makeTableLALR")
  let
    parserName = head[0]
    tokenType = head[1]
    tokenKind = parseStmt(tokenType.strVal & "Kind")[0]
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
  nimyInfo["__Start__"] = initNimyRow(NonTerm,
                                      rtn = returnType, rtp = genSym())

  # make opt and rep
  var optAndRep: seq[NimNode] = @[]
  for clause in body:
    if clause.kind == nnkCommentStmt:
      continue
    for ruleClause in clause[1]:
      if ruleClause.kind == nnkCommentStmt:
        continue
      for sym in ruleClause.ruleRight:
        if sym.isTerm(nimyInfo) and not(nimyInfo.haskey(sym.strVal)):
          nimyInfo[sym.strVal] = initNimyRow(Term)
        if not (sym.kind in {nnkBracketExpr, nnkCurlyExpr}):
          continue
        doAssert sym.len == 1
        let innerSym = sym[0].strVal
        if sym[0].isTerm(nimyInfo) and
           not(nimyInfo.haskey(innersym)):
          nimyInfo[innerSym] = initNimyRow(Term)
        case sym.kind
        of nnkBracketExpr:
          if nimyInfo[innerSym].optRule.kind != nnkEmpty:
            continue
          let
            newStr = "__opt_" & innerSym
            new = newIdentNode(newStr)
            ty = if innerSym.isNonTerm(nimyInfo):
                   nimyInfo[innerSym].retTyNode
                 else:
                   tokenType
            rt = nnkBracketExpr.newTree(
              newIdentNode("seq"),
              ty
            )
            nr = nimyInfo[innerSym]

          optAndRep.add(getOpt(newIdentNode(innerSym), ty, new))
          nimyInfo[newStr] = initNimyRow(NonTerm, rtn = rt, rtp = genSym())
          nimyInfo[innerSym] = NimyRow(
            kind: nr.kind,
            retTyNode: nr.retTyNode,
            ruleToProc: nr.ruleToProc,
            optRule: new,
            repRule: nr.repRule
            )

        of nnkCurlyExpr:
          if nimyInfo[innerSym].optRule.kind != nnkEmpty:
            continue
          let
            newStr = "__rep_" & innerSym
            new = newIdentNode(newStr)
            newInnerStr = "__inner_" & newStr
            newInner = newIdentNode(newInnerStr)
            ty = if innerSym.isNonTerm(nimyInfo):
                   nimyInfo[innerSym].retTyNode
                 else:
                   tokenType
            rt = nnkBracketExpr.newTree(
              newIdentNode("seq"),
              ty
            )
            nr = nimyInfo[innerSym]

          optAndRep.add(getRep(newIdentNode(innerSym), ty, new, newInner))
          nimyInfo[newStr] = initNimyRow(NonTerm, rtn = rt,
                                         rtp = genSym())
          nimyInfo[newInnerStr] = initNimyRow(NonTerm, rtn = rt,
                                              rtp = genSym())
          nimyInfo[innerSym] = NimyRow(
            kind: nr.kind,
            retTyNode: nr.retTyNode,
            ruleToProc: nr.ruleToProc,
            optRule: nr.optRule,
            repRule: new
            )

        else:
          discard

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

  # read BNF second (make procs)
  for i, clause in iter(topClause, body, optAndRep):
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
        # argTypes: seq[string] (name if nonterm)
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
  symNodes.add(
    newCall(
      nnkBracketExpr.newTree(
        newIdentNode("Empty"),
        tokenKind
      )
    )
  )
  let
    tmpName = genSym(nskProc)
  result.add(
    tableMakerProc(tmpName, tokenType, tokenKind, topNonTermNode, tableMaker,
                   ruleIds, ruleDefs, symNodes)
  )
  var tableName: NimNode
  when defined(nimylet):
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

  let
    its = genSym(nskVar)
    itr = genSym(nskVar)

  result.add(ruleDefs)

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
        tableName
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
