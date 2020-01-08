import tables
import hashes
import sets

import patty

import parsetypes
import parser

type
  LRItem*[T] = object
    rule*: Rule[T]
    pos*: int

  LRItems*[T] = HashSet[LRItem[T]]
  SetOfLRItems*[T] = OrderedSet[LRItems[T]]

  TransTable*[T] = seq[Table[Symbol[T], int]]

proc initTransTableRow[T](): Table[Symbol[T], int] =
  result = initTable[Symbol[T], int]()

proc indexOf[T](os: OrderedSet[T], element: T): int =
  for i, key in os:
    if key == element:
      return i
  return -1

proc `$`*[T](s: SetOfLRItems[T]): string =
  result = "CanonicalCollection:\n--------\n"
  for i, itms in s:
    result = result & $i & ":" & $itms & "\n"
  result = result & "--------\n"

proc next[T](i: LRItem[T]): Symbol[T] =
  if i.pos >= i.rule.len:
    return End[T]()
  result = i.rule.right[i.pos]

proc nextSkipEmpty[T](i: LRItem[T]): Symbol[T] =
  result = End[T]()
  for idx in i.pos..<i.rule.len:
    let nxt = i.rule.right[idx]
    if nxt != Empty[T]():
      result = nxt
      break

proc pointForward[T](i: LRItem[T]): LRItem[T] =
  doAssert i.pos < i.rule.len
  result = LRItem[T](rule: i.rule, pos: i.pos + 1)

proc closure[T](g: Grammar[T], whole: LRItems[T]): LRItems[T] =
  result = whole
  var checkSet = whole
  while checkSet.len > 0:
    var new: LRItems[T]
    new.init()
    for i in checkSet:
      match i.next:
        NonTermS:
          for r in g.filterRulesLeftIs(i.next):
            let n = LRItem[T](rule: r, pos: 0)
            if not result.containsOrIncl(n):
              new.incl(n)
        _:
          discard
    checkSet = new

proc goto[T](g: Grammar[T], itms: LRItems[T], s: Symbol[T]): LRItems[T] =
  doAssert s.kind != SymbolKind.End
  assert itms == g.closure(itms)
  var gotoHashSet = initHashSet[LRItem[T]]()
  for i in itms:
    if i.next == s:
      gotoHashSet.incl(i.pointForward)
  result = g.closure(gotoHashSet)

proc hash*[T](x: LRItem[T]): Hash =
  var h: Hash = 0
  h = h !& hash(x.rule)
  h = h !& hash(x.pos)
  return !$h

proc makeCanonicalCollection*[T](g: Grammar[T]): (SetOfLRItems[T],
                                                  TransTable[T]) =
  let init = g.closure([LRItem[T](rule: g.startRule, pos: 0)].toHashSet)
  var
    cc = [
      init
    ].toOrderedSet
    checkSet = cc
    tt: TransTable[T] = @[]
  tt.add(initTransTableRow[T]())
  while checkSet.len > 0:
    var new: SetOfLRItems[T]
    new.init()
    for itms in checkSet:
      let frm = cc.indexOf(itms)
      assert itms == g.closure(itms)
      var done = initHashSet[Symbol[T]]()
      done.incl(End[T]())
      for i in itms:
        let s = i.next
        if (not done.containsOrIncl(s)):
          let gt = goto[T](g, itms, s)
          if (not cc.containsOrIncl(gt)):
            tt.add(initTransTableRow[T]())
            assert cc.card == tt.len
            new.incl(gt)
          tt[frm][s] = cc.indexOf(gt)
    checkSet = new
  doAssert cc.indexOf(init) == 0, "init state is not '0'"
  result = (cc, tt)

proc makeTableLR*[T](g: Grammar[T]): ParsingTable[T] =
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
    (canonicalCollection, _) = makeCanonicalCollection[T](ag)
  for idx, itms in canonicalCollection:
    actionTable[idx] = initTable[Symbol[T], ActionTableItem[T]]()
    gotoTable[idx] = initTable[Symbol[T], State]()
    for item in itms:
      let sym = item.nextSkipEmpty
      match sym:
        TermS:
          let i = canonicalCollection.indexOf(ag.goto(itms, sym))
          assert i > -1,"There is no 'items' which is equal to 'goto'"
          when defined(nimydebug):
            if actionTable[idx].haskey(sym) and
              actionTable[idx][sym].kind == ActionTableItemKind.Reduce:
              echo "LR:CONFLICT!!!" & $idx & ":" & $sym
          actionTable[idx][sym] = Shift[T](i)
        NonTermS:
          let i = canonicalCollection.indexOf(ag.goto(itms, sym))
          assert i > -1, "There is no 'items' which is equal to 'goto'"
          gotoTable[idx][sym] = i
        End:
          if item.rule.left == ag.start:
            actionTable[idx][End[T]()] = Accept[T]()
          else:
            for flw in ag.followTable[item.rule.left]:
              if flw.kind == SymbolKind.TermS or flw.kind == SymbolKind.End:
                if actionTable[idx].haskey(flw) and
                   actionTable[idx][flw].kind == ActionTableItemKind.Shift:
                  when defined(nimydebug):
                    echo "LR:CONFLICT!!!" & $idx & ":" & $flw
                  continue
                actionTable[idx][flw] = Reduce[T](item.rule)
        _:
          when defined(nimy_debug):
            echo "LR: OTHER (" & $sym & ")"
          discard
  result = ParsingTable[T](action: actionTable, goto: gotoTable)
  when defined(nimydebug):
    echo "LR:"
    echo ag.followTable
    echo canonicalCollection
    echo result

proc filterKernel*[T](cc: SetOfLRItems[T]): SetOfLRItems[T] =
  result = initOrderedSet[LRItems[T]]()
  let start = NonTermS[T]("__Start__")
  for i, itms in cc:
    for itm in itms:
      var kernelItems = initHashSet[LRItem[T]]()
      for itm in itms:
        if itm.pos != 0 or itm.rule.left == start:
          kernelItems.incl(itm)
      result.incl(kernelItems)
