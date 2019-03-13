import tables
import nimltype
import hashes
import sets
import algorithm

import lexer

nimltype Symbol:
  TermS of sym
  NonTermS of sym
  End
  Nil
  Empty

type
  State = int
  Rule = object
    left: Symbol
    right: seq[Symbol]
  LALRItem = object
    rule: Rule
    pos: int

  Grammar = object
    rules: HashSet[Rule]
    start: Symbol
    firstTable: FirstTable
    followTable: FollowTable

  FollowTable = Table[Symbol, HashSet[Symbol]]
  FirstTable = Table[Symbol, HashSet[Symbol]]

  LALRItems = HashSet[LALRItem]
  SetOfLALRItems = OrderedSet[LALRItems]

proc hash[T](x: HashSet[T]): Hash =
  result = 0
  var s: seq[Hash] = @[]
  for i in x:
    s.add(i.hash)
  for h in s.sorted(cmp[Hash]):
    result = result !& h
  result = !$result

proc hash(x: Symbol): Hash =
  var h: Hash = 0
  h = h !& hash(x.kind)
  match x:
    TermS s:
      h = h !& hash(s)
    NonTermS s:
      h = h !& hash(s)
    else:
      discard
  return !$h

proc hash(x: Rule): Hash =
  var h: Hash = 0
  h = h !& hash(x.left)
  h = h !& hash(x.right)
  return !$h

proc hash(x: LALRItem): Hash =
  var h: Hash = 0
  h = h !& hash(x.rule)
  h = h !& hash(x.pos)
  return !$h

nimltype Option[T]:
  None
  Some of T

nimltype ActionTableItem:
  Shift of State
  Reset of Rule
  Accept
  Error

type
  Parser = seq[State]
  ActionRow = Table[Symbol, ActionTableItem]
  ActionTable = Table[State, ActionRow]
  GotoRow = Table[Symbol, State]
  GotoTable = Table[State, GotoRow]
  ParsingTable = object
    action: ActionTable
    goto: GotoTable

nimltype ParseTree[T]:
  Terminal of Token[T]
  NonTerminal of (Rule, seq[ParseTree[T]])

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

proc len(r: Rule): int =
  return r.right.len

proc push(parser: var Parser, s: State) =
  parser.add(s)

proc top(parser: Parser): State =
  return parser[parser.high]

proc parse[T](
    parser: var Parser, parsingTable: ParsingTable,
    lexer: Lexer): ParseTree[T] =
  var tree: seq[ParseTree[T]] = @[]
  var token: lexer.next
  while true:
    match parsingTable.action[parser.top][token.symbol]:
      Shift s:
        tree.add(Terminal(token))
        token = lexer.next
      Reset r:
        let reseted = tree[^r.len..^1]
        for i in 0..<r.len:
          discard parser.pop
          discard tree.pop
        tree.add(NonTerminal(r, reseted))
        parser.add(ParsingTable.goto[parser.top][r])
      Accept:
        assert tree.len == 1, "Error, parsing result is wrong."
        return tree[0]
      Error:
        assert false, "Error, Must be implemented."
  assert false, "Something wrong with parser."

proc newRule(left: Symbol, right: seq[Symbol]): Rule =
  assert left.kind == SymbolKind.NonTermS,
     "Right side of rule must be Non-Terminal Symbol."
  result = Rule(left: left, right: right)

proc initGrammar(rules: HashSet[Rule], start: Symbol): Grammar =
  result = Grammar(rules: rules, start: start)

proc initGrammar(rules: openArray[Rule], start: Symbol): Grammar =
  result = initGrammar(rules.toSet, start)

proc filterRulesLeftIs(g: Grammar, x: Symbol): seq[Rule] =
  result = @[]
  for r in g.rules:
    if r.left == x:
      assert (not (r in result)), "x in result."
      result.add(r)

proc isAugument(g: Grammar): bool =
  result = (g.start == NonTermS("__Start__"))
  assert (g.filterRulesLeftIs(g.start).len == 0),
     "`g` is invalid augument gramer."

proc startRule(g: Grammar): Rule =
  doAssert g.isAugument, "`g` is not augument gramer."
  let ret = g.filterRulesLeftIs(g.start)
  doAssert (ret.len == 1), "`g` is invalid augument gramer."
  for r in ret:
    result = r

proc next(i: LALRItem): Symbol =
  if i.pos >= i.rule.len:
    return Nil()
  result = i.rule.right[i.pos]

proc prev(i: LALRItem): Symbol =
  if i.pos == 0:
    return Nil()
  result = i.rule.right[i.pos - 1]

proc pointForward(i: LALRItem): LALRItem =
  doAssert i.pos < i.rule.len
  result = LALRItem(rule: i.rule, pos: i.pos + 1)

proc closure(g: Grammar, whole: LALRItems): LALRItems =
  var checkSet = result
  while checkSet.len > 0:
    var new: LALRItems
    for i in checkSet:
      new.init()
      match i.next:
        NonTermS:
          for r in g.filterRulesLeftIs(i.next):
            let n = LALRItem(rule: r, pos: 0)
            if not result.containsOrIncl(n):
              new.incl(n)
        else:
          discard
    checkSet = new

proc goto(g: Grammar, itms: LALRItems, s: Symbol): LALRItems =
  doAssert s.kind != SymbolKind.Nil
  assert itms == g.closure(itms)
  var gotoSet = initSet[LALRItem]()
  for i in itms:
    if i.next == s:
      gotoSet.incl(i.pointForward)
  result = g.closure(gotoSet)

proc makeCanonicalCollection(g: Grammar): SetOfLALRItems =
  let init = g.closure([LALRItem(rule: g.startRule, pos: 0)].toSet)
  result = [
    init
  ].toOrderedSet
  var checkSet = result
  while checkSet.len > 0:
    var new: SetOfLALRItems
    for itms in checkSet:
      new.init()
      assert itms == g.closure(itms)
      var done = initSet[Symbol]()
      done.incl(Nil())
      for i in itms:
        let s = i.next
        if (not done.containsOrIncl(s)):
          let gt = g.goto(itms, s)
          if (not result.containsOrIncl(gt)):
            new.incl(gt)
    checkSet = new
  doAssert result.indexOf(init) == 0, "init state is not '0'"

proc symbolSet(g: Grammar): HashSet[Symbol] =
  result.init()
  for r in g.rules:
    for s in r.right:
      result.incl(s)
  result.incl(g.start)

proc termSymbolSet(g: Grammar): HashSet[Symbol] =
  result.init()
  for r in g.rules:
    for s in r.right:
      if s.kind == SymbolKind.TermS:
        result.incl(s)
  result.incl(g.start)

proc nonTermSymbolSet(g: Grammar): HashSet[Symbol] =
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

proc makeFirstTable(g: Grammar): FirstTable =
  result = initTable[Symbol, HashSet[Symbol]]()
  for s in g.symbolSet:
    match s:
      NonTermS _:
        var initSet: HashSet[Symbol]
        initSet.init()
        result[s] = initSet
      TermS _:
        result[s] = [s].toSet
      else:
        doAssert false, "There is a non-symbol in rules."

  for r in g.rules:
    if r.right.len == 0:
      result[r.left].incl(Empty())

  var fCnt = true
  while fCnt:
    fCnt = false
    for r in g.rules:
      var fEmp = true
      for s in r.right:
        let newFst = result[r.left] + (result[s] - [Empty()].toSet)
        if fCnt or result[r.left] == newFst:
          fCnt = true
        result[r.left] = newFst
        if not result[s].contains(Empty()):
          fEmp = false
          break
      if fEmp:
        if not result[r.left].containsOrIncl(Empty()):
          fCnt = true

proc makeFollowTable(g: Grammar): FollowTable =
  doAssert g.firstTable != nil, "firstTable is nill."
  result = initTable[Symbol, HashSet[Symbol]]()
  for s in g.nonTermSymbolSet:
    var initSet: HashSet[Symbol]
    initSet.init()
    result[s] = initSet
  result[g.start].incl(Nil())
  var fCnt = true
  while fCnt:
    fCnt = false
    for r in g.rules:
      var
        fEmpTail = true
        firstSyms: HashSet[Symbol]
      firstSyms.init()
      # for sym in r.right.reversed
      for i in countdown(r.right.len - 1, 0):
        let sym = r.right[i]
        match sym:
          TermS _:
            # renew meta data
            fEmpTail = false
            firstSyms = [sym].toSet
          NonTermS _:
            # renew first table
            let fsts = g.firstTable[sym]
            fCnt = (not result[sym].containsOrIncl(firstSyms)) or fCnt
            if fEmpTail:
              fCnt = (not result[sym].containsOrincl(result[r.left])) or fCnt

            # renew meta data
            if fsts.contains(Empty()):
              firstSyms.incl(fsts)
            else:
              fEmpTail = false
              firstSyms = fsts
          else:
            doAssert false, "There is other than Term or NonTerm in Rules."

proc augument(g: Grammar): Grammar =
  let
    start = NonTermS("__Start__")
    startRule = newRule(left = start, right = @[g.start])
  assert (not g.rules.contains(startRule)),
     "`g` is already augument. (the sym '__Start__' can't be used.)"
  var singleStart = initSet[Rule]()
  singleStart.incl(startRule)
  let newRules = g.rules + singleStart
  result = initGrammar(newRules, start)
  result.firstTable = g.makeFirstTable
  result.followTable = g.makeFollowTable

proc makeTable(g: Grammar): ParsingTable =
  var
    actionTable: ActionTable
    gotoTable: GotoTable
  actionTable = initTable[State, ActionRow]()
  gotoTable = initTable[State, GotoRow]()
  let
    ag = if g.isAugument:
           g
         else:
           g.augument
    canonicalCollection = ag.makeCanonicalCollection
  for idx, itms in canonicalCollection:
    var
      aRow = initTable[Symbol, ActionTableItem]()
      gRow = initTable[Symbol, State]()
    actionTable[idx] = aRow
    gotoTable[idx] = gRow
    for item in itms:
      let sym = item.next
      match sym:
        TermS _:
          let i = canonicalCollection.indexOf(g.goto(itms, sym))
          assert i > -1,"There is no 'items' which is equal to 'goto'"
          actionTable[idx][sym] = Shift(i)
        NonTermS _:
          let i = canonicalCollection.indexOf(g.goto(itms, sym))
          assert i > -1, "There is no 'items' which is equal to 'goto'"
          gotoTable[idx][sym] = i
        Nil:
          if item.rule.left == ag.start:
            actionTable[idx][End()] = Accept()
          else:
            let prev = item.prev
            if prev.kind == SymbolKind.NonTermS:
              for flw in g.followTable[item.rule.left]:
                if flw.kind == SymbolKind.TermS:
                    actionTable[idx][flw] = Reset(item.rule)
        else:
          discard
  result.action = actionTable
  result.goto = gotoTable

proc initParser(): Parser =
  result = @[0]
