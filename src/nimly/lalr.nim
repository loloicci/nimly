import tables
import hashes
import sets
import algorithm
import strutils

import patty

import lextypes
import lexer

type
  sym* = string

type
  SymbolKind {.pure.} = enum
    TermS
    NonTermS
    End
    Nil
    Empty

  Symbol*[T] = object
    case kind*: SymbolKind
    of SymbolKind.TermS:
      term*: T
    of SymbolKind.NonTermS:
      nonTerm*: sym
    else:
      discard

proc `==`*[T](a, b: Symbol[T]): bool =
  if a.kind != b.kind:
    return false
  match a:
    TermS(term: t):
      return t == b.term
    NonTermS(nonTerm: nt):
      return nt == b.nonTerm
    _:
      return true

proc NonTermS*[T](nonTerm: sym): Symbol[T] =
  return Symbol[T](kind: SymbolKind.NonTermS, nonTerm: nonTerm)

proc Nil*[T](): Symbol[T] =
  return Symbol[T](kind: SymbolKind.Nil)

proc End*[T](): Symbol[T] =
  return Symbol[T](kind: SymbolKind.End)

proc Empty*[T](): Symbol[T] =
  return Symbol[T](kind: SymbolKind.Empty)

proc TermS*[T](term: T): Symbol[T] =
  return Symbol[T](kind: SymbolKind.TermS, term: term)

type
  State = int
  Rule*[T] = object
    left*: Symbol[T]
    right*: seq[Symbol[T]]
  LALRItem[T] = object
    rule: Rule[T]
    pos: int

  Grammar[T] = object
    rules: HashSet[Rule[T]]
    start: Symbol[T]
    firstTable: FirstTable[T]
    followTable: FollowTable[T]

  FollowTable[T] = Table[Symbol[T], HashSet[Symbol[T]]]
  FirstTable[T] = Table[Symbol[T], HashSet[Symbol[T]]]

  LALRItems[T] = HashSet[LALRItem[T]]
  SetOfLALRItems[T] = OrderedSet[LALRItems[T]]

proc hash*[T](x: Symbol[T]): Hash =
  var h: Hash = 0
  h = h !& hash(x.kind)
  match x:
    TermS(term: s):
      h = h !& hash(s)
    NonTermS(nonTerm: s):
      h = h !& hash(s)
    _:
      discard
  return !$h

proc hash*[T](x: Rule[T]): Hash =
  var h: Hash = 0
  h = h !& hash(x.left)
  h = h !& hash(x.right)
  return !$h

proc hash*[T](x: LALRItem[T]): Hash =
  var h: Hash = 0
  h = h !& hash(x.rule)
  h = h !& hash(x.pos)
  return !$h

variant Option[T]:
  None
  Some(value: T)

type
  ActionTableItemKind {.pure.} = enum
    Shift
    Reset
    Accept
    Error
  ActionTableItem[T] = object
    case kind: ActionTableItemKind:
    of ActionTableItemKind.Shift:
      state: State
    of ActionTableItemKind.Reset:
      rule: Rule[T]
    else:
      discard

proc `$`*[T](i: ActionTableItem[T]): string =
  match i:
    Shift(state: s):
      return "Shift(" & $s & ")"
    Reset(rule: r):
      return "Reset(" & $r & ")"
    Accept:
      return "Accept"
    Error:
      return "Error"

proc Shift[T](state: State): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Shift, state: state)

proc Reset[T](rule: Rule[T]): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Reset, rule: rule)

proc Accept[T](): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Accept)

proc Error[T](): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Error)

type
  ActionRow[T] = Table[Symbol[T], ActionTableItem[T]]
  ActionTable[T] = Table[State, ActionRow[T]]
  GotoRow[T] = Table[Symbol[T], State]
  GotoTable[T] = Table[State, GotoRow[T]]
  ParsingTable*[T] = object
    action*: ActionTable[T]
    goto*: GotoTable[T]
  ConstActionTable* = seq[seq[int]]
  ConstGotoTable* = seq[seq[int]]
  ConstTable* = (ConstActionTable, ConstGotoTable)
  SymbolToInt*[T] = Table[Symbol[T], int]
  RuleToInt*[T] = Table[Rule[T], int]
  Parser*[T] = object
    stack: seq[State]
    table*: ParsingTable[T]
  IntToSym*[T] = Table[int, Symbol[T]]
  IntToRule*[T] = Table[int, Rule[T]]

variantp ParseTree[T, S]:
  Terminal(token: T)
  NonTerminal(rule: Rule[S], tree: seq[ParseTree[T, S]])

proc inst[T](sq: var seq[T], item: T, default: T, index: int) =
  for i in 0..(index - sq.len):
    sq.add(default)
  sq[index] = item

proc toConst*[T](pt: ParsingTable[T],
                 sti: SymbolToInt[T],
                 rti: RuleToInt[T]): ConstTable =
  var
    cat: seq[seq[int]] = @[]
    cgt: seq[seq[int]] = @[]
  for state, ar in pt.action:
    var row: seq[int] = @[]
    for sym, ati in ar:
      let idx = sti[sym]
      var item: int
      case ati.kind
      of ActionTableItemKind.Shift:
        let s = ati.state
        item = s
      of ActionTableItemKind.Reset:
        let r = ati.rule
        item = rti[r]
      of ActionTableItemKind.Accept:
        item = high(int)
      else:
        doAssert false
      row.inst(item, low(int), idx)
    let default: seq[int] = @[]
    cat.inst(row, default, state)

  for state, gr in pt.goto:
    var row: seq[int] = @[]
    for sym, st in gr:
      let idx = sti[sym]
      row.inst(st, low(int), idx)
    let default: seq[int] = @[]
    cgt.inst(row, default, state)

  result = (cat, cgt)

proc reconstruct*[T](cpt: ConstTable,
                     its: IntToSym[T],
                     itr: IntToRule[T]): ParsingTable[T] =
  let (cat, cgt) = cpt
  var
    rat = initTable[State, ActionRow[T]]()
    rgt = initTable[State, GotoRow[T]]()
  for rid, row in cat:
    if row.len == 0:
      continue
    var trow = initTable[Symbol[T], ActionTableItem[T]]()
    for id, ati in row:
      if ati == high(int):
        trow[its[id]] = Accept[T]()
      elif ati < 0 and ati != low(int):
        trow[its[id]] = Reset[T](itr[ati])
      elif ati > 0:
        trow[its[id]] = Shift[T](ati)
    rat[rid] = trow
  for rid, row in cgt:
    if row.len == 0:
      continue
    var trow = initTable[Symbol[T], State]()
    for id, st in row:
      if st == low(int):
        continue
      trow[its[id]] = st
    rgt[rid] = trow
  result = ParsingTable[T](action: rat, goto: rgt)

proc `$`*[T, S](pt: ParseTree[T, S], indent: int = 0): string =
  match pt:
    Terminal(token: t):
      result = "  ".repeat(indent) & $t & "\n"
    NonTerminal(rule: r, tree: t):
      result = "  ".repeat(indent) & "rule: " & $r.right & "\n"
      for n in t:
        result = result & `$`(n, indent + 1)

proc `[]`[T](os: OrderedSet[T], idx: int): T {.inline.} =
  if os.len <= idx:
    raise newException(IndexError, "idx is too large.")
  for i, key in os:
    if i == idx:
      return key

proc indexOf[T](os: OrderedSet[T], element: T): int =
  for i, key in os:
    if key == element:
      return i
  return -1

proc len[T](r: Rule[T]): int =
  return r.right.len

proc add[T](parser: var Parser[T], s: State) =
  parser.stack.add(s)

proc push[T](parser: var Parser[T], s: State) =
  parser.add(s)

proc pop[T](parser: var Parser[T]): State =
  return parser.stack.pop

proc top[T](parser: Parser[T]): State =
  return parser.stack[parser.stack.high]

proc parseImpl*[T, S](parser: var Parser[S],
                      lexer: var NimlLexer[T]): ParseTree[T, S] =
  var tree: seq[ParseTree[T, S]] = @[]
  var token = lexer.lexNext
  var symbol = TermS[S](token.kind)
  while true:
    when defined(nimydebug):
      echo parser.stack
      echo symbol
    let action = parser.table.action[parser.top][symbol]
    when defined(nimydebug):
      echo action
    case action.kind
    of ActionTableItemKind.Shift:
      let s = action.state
      tree.add(Terminal[T, S](token))
      try:
        token = lexer.lexNext
        symbol = TermS[S](token.kind)
      except NimlEOFError:
        symbol = Nil[S]()
      except:
        echo "unexpected"
        raise
      parser.push(s)
    of ActionTableItemKind.Reset:
      let r = action.rule
      let reseted = tree[^r.len..^1]
      for i in 0..<r.len:
        discard parser.pop
        discard tree.pop
      tree.add(NonTerminal[T, S](r, reseted))
      parser.push(parser.table.goto[parser.top][r.left])
    of ActionTableItemKind.Accept:
      when defined(nimydebug):
        if tree.len == 1:
          echo tree[0]
        else:
          echo tree
      doAssert tree.len == 1, "Error, parsing result is wrong."
      return tree[0]
    of ActionTableItemKind.Error:
      doAssert false, "Error, Must be implemented."
  assert false, "Something wrong with parser."

proc newRule*[T](left: Symbol[T], right: varargs[Symbol[T]]): Rule[T] =
  assert left.kind == SymbolKind.NonTermS,
     "Right side of rule must be Non-Terminal Symbol."
  var rightSeq: seq[Symbol[T]] = @[]
  for s in right:
    rightSeq.add(s)

  result = Rule[T](left: left, right: rightSeq)

proc newRule*[T](left: Symbol[T], right: Symbol[T]): Rule[T] =
  assert left.kind == SymbolKind.NonTermS,
     "Right side of rule must be Non-Terminal Symbol."
  result = Rule[T](left: left, right: @[right])

proc initGrammar*[T](rules: HashSet[Rule[T]], start: Symbol[T]): Grammar[T] =
  result = Grammar[T](rules: rules, start: start)

proc initGrammar*[T](rules: openArray[Rule[T]],
                     start: Symbol[T]): Grammar[T] =
  result = initGrammar(rules.toSet, start)

proc filterRulesLeftIs[T](g: Grammar[T], x: Symbol[T]): seq[Rule[T]] =
  result = @[]
  for r in g.rules:
    if r.left == x:
      assert (not (r in result)), "x in result."
      result.add(r)

proc isAugument[T](g: Grammar[T]): bool =
  result = (g.start == NonTermS[T]("__Start__"))
  assert (g.filterRulesLeftIs(g.start).len != 0),
     "`g` is invalid gramer."

proc startRule[T](g: Grammar[T]): Rule[T] =
  doAssert g.isAugument, "`g` is not augument gramer."
  let ret = g.filterRulesLeftIs(g.start)
  doAssert (ret.len == 1), "`g` is invalid augument gramer."
  for r in ret:
    result = r

proc next[T](i: LALRItem[T]): Symbol[T] =
  if i.pos >= i.rule.len:
    return Nil[T]()
  result = i.rule.right[i.pos]

proc pointForward[T](i: LALRItem[T]): LALRItem[T] =
  doAssert i.pos < i.rule.len
  result = LALRItem[T](rule: i.rule, pos: i.pos + 1)

proc closure[T](g: Grammar[T], whole: LALRItems[T]): LALRItems[T] =
  result = whole
  var checkSet = whole
  while checkSet.len > 0:
    var new: LALRItems[T]
    new.init()
    for i in checkSet:
      match i.next:
        NonTermS:
          for r in g.filterRulesLeftIs(i.next):
            let n = LALRItem[T](rule: r, pos: 0)
            if not result.containsOrIncl(n):
              new.incl(n)
        _:
          discard
    checkSet = new

proc goto[T](g: Grammar[T], itms: LALRItems[T], s: Symbol[T]): LALRItems[T] =
  doAssert s.kind != SymbolKind.Nil
  assert itms == g.closure(itms)
  var gotoSet = initSet[LALRItem[T]]()
  for i in itms:
    if i.next == s:
      gotoSet.incl(i.pointForward)
  result = g.closure(gotoSet)

proc makeCanonicalCollection[T](g: Grammar[T]): SetOfLALRItems[T] =
  let init = g.closure([LALRItem[T](rule: g.startRule, pos: 0)].toSet)
  result = [
    init
  ].toOrderedSet
  var checkSet = result
  while checkSet.len > 0:
    var new: SetOfLALRItems[T]
    new.init()
    for itms in checkSet:
      assert itms == g.closure(itms)
      var done = initSet[Symbol[T]]()
      done.incl(Nil[T]())
      for i in itms:
        let s = i.next
        if (not done.containsOrIncl(s)):
          let gt = goto[T](g, itms, s)
          if (not result.containsOrIncl(gt)):
            new.incl(gt)
    checkSet = new
  doAssert result.indexOf(init) == 0, "init state is not '0'"

proc symbolSet[T](g: Grammar[T]): HashSet[Symbol[T]] =
  result.init()
  for r in g.rules:
    for s in r.right:
      result.incl(s)
  result.incl(g.start)

proc nonTermSymbolSet[T](g: Grammar[T]): HashSet[Symbol[T]] =
  result.init()
  for r in g.rules:
    for s in r.right:
      if s.kind == SymbolKind.NonTermS:
        result.incl(s)
  result.incl(g.start)

proc containsOrIncl[T](s: var HashSet[T], other: HashSet[T]): bool =
  result = true
  assert s.isValid, "The set `s` needs to be initialized."
  assert other.isValid, "The set `other` needs to be initialized."
  for item in other:
    result = result and containsOrIncl(s, item)

proc makeFirstTable[T](g: Grammar[T]): FirstTable[T] =
  result = initTable[Symbol[T], HashSet[Symbol[T]]]()
  for s in g.symbolSet:
    match s:
      NonTermS:
        var initSet: HashSet[Symbol[T]]
        initSet.init()
        result[s] = initSet
      TermS:
        result[s] = [s].toSet
      _:
        doAssert false, "There is a non-symbol in rules."

  for r in g.rules:
    if r.right.len == 0:
      result[r.left].incl(Empty[T]())

  var fCnt = true
  while fCnt:
    fCnt = false
    for r in g.rules:
      var fEmp = true
      for s in r.right:
        let newFst = result[r.left] + (result[s] - [Empty[T]()].toSet)
        if result[r.left] != newFst:
          fCnt = true
        result[r.left] = newFst
        if not result[s].contains(Empty[T]()):
          fEmp = false
          break
      if fEmp:
        if not result[r.left].containsOrIncl(Empty[T]()):
          fCnt = true

proc makeFollowTable[T](g: Grammar[T]): FollowTable[T] =
  doAssert g.firstTable.len != 0, "firstTable is nill."
  result = initTable[Symbol[T], HashSet[Symbol[T]]]()
  for s in g.nonTermSymbolSet:
    var initSet: HashSet[Symbol[T]]
    initSet.init()
    result[s] = initSet
  result[g.start].incl(Nil[T]())
  var fCnt = true
  while fCnt:
    fCnt = false
    for r in g.rules:
      var
        fEmpTail = true
        firstSyms: HashSet[Symbol[T]]
      firstSyms.init()
      # for sym in r.right.reversed
      for i in countdown(r.right.len - 1, 0):
        let sym = r.right[i]
        assert sym != Nil[T]()
        match sym:
          TermS:
            # renew meta data
            fEmpTail = false
            firstSyms = [sym].toSet
          NonTermS:
            # renew first table
            for f in firstSyms:
              let prevFC = fCnt
              fCnt = (not result[sym].containsOrIncl(f))
              fCnt = fCnt or prevFC
            if fEmpTail:
              for f in result[r.left]:
                let prevFC = fCnt
                fCnt = (not result[sym].containsOrincl(f))
                fCnt = fCnt or prevFC

            # renew meta data
            let fsts = g.firstTable[sym]
            if fsts.contains(Empty[T]()):
              for f in fsts:
                firstSyms.incl(f)
            else:
              fEmpTail = false
              firstSyms = fsts
          _:
            doAssert false, "There is other than Term or NonTerm in Rules."

proc augument[T](g: Grammar[T]): Grammar[T] =
  let
    start = NonTermS[T]("__Start__")
    startRule = newRule(left = start, right = g.start)
  assert (not g.rules.contains(startRule)),
     "`g` is already augument. (the sym '__Start__' can't be used.)"
  var singleStart = initSet[Rule[T]]()
  singleStart.incl(startRule)
  let newRules = g.rules + singleStart
  result = initGrammar(newRules, start)
  result.firstTable = result.makeFirstTable
  result.followTable = result.makeFollowTable

proc makeTable*[T](g: Grammar[T]): ParsingTable[T] =
  var
    actionTable: ActionTable[T]
    gotoTable: GotoTable[T]
  actionTable = initTable[State, ActionRow[T]]()
  gotoTable = initTable[State, GotoRow[T]]()
  let
    ag = if g.isAugument:
           g
         else:
           g.augument
    canonicalCollection = makeCanonicalCollection[T](ag)
  for idx, itms in canonicalCollection:
    actionTable[idx] = initTable[Symbol[T], ActionTableItem[T]]()
    gotoTable[idx] = initTable[Symbol[T], State]()
    for item in itms:
      let sym = item.next
      match sym:
        TermS:
          let i = canonicalCollection.indexOf(ag.goto(itms, sym))
          assert i > -1,"There is no 'items' which is equal to 'goto'"
          actionTable[idx][sym] = Shift[T](i)
        NonTermS:
          let i = canonicalCollection.indexOf(ag.goto(itms, sym))
          assert i > -1, "There is no 'items' which is equal to 'goto'"
          gotoTable[idx][sym] = i
        Nil:
          if item.rule.left == ag.start:
            actionTable[idx][Nil[T]()] = Accept[T]()
          else:
            for flw in ag.followTable[item.rule.left]:
              if flw.kind == SymbolKind.TermS or flw.kind == SymbolKind.Nil:
                actionTable[idx][flw] = Reset[T](item.rule)
        _:
          discard
  result = ParsingTable[T](action: actionTable, goto: gotoTable)

proc newParser*[T](t: ParsingTable[T]): Parser[T] =
  result = Parser[T](stack: @[0], table: t)

proc initParser*[T](p: var Parser[T]) =
  p.stack = @[0]
