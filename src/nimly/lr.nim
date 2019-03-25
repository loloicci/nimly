import tables
import hashes
import sets
import algorithm
import strutils

import patty

import parsetypes
import parser
import lextypes
import lexer

type
  LRItem*[T] = object
    rule*: Rule[T]
    pos*: int

  State = int

  LRItems[T] = HashSet[LRItem[T]]
  SetOfLRItems[T] = OrderedSet[LRItems[T]]

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

proc next*[T](i: LRItem[T]): Symbol[T] =
  if i.pos >= i.rule.len:
    return Nil[T]()
  result = i.rule.right[i.pos]

proc pointForward*[T](i: LRItem[T]): LRItem[T] =
  doAssert i.pos < i.rule.len
  result = LRItem[T](rule: i.rule, pos: i.pos + 1)

proc closure*[T](g: Grammar[T], whole: LRItems[T]): LRItems[T] =
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

proc goto*[T](g: Grammar[T], itms: LRItems[T], s: Symbol[T]): LRItems[T] =
  doAssert s.kind != SymbolKind.Nil
  assert itms == g.closure(itms)
  var gotoSet = initSet[LRItem[T]]()
  for i in itms:
    if i.next == s:
      gotoSet.incl(i.pointForward)
  result = g.closure(gotoSet)

proc hash*[T](x: LRItem[T]): Hash =
  var h: Hash = 0
  h = h !& hash(x.rule)
  h = h !& hash(x.pos)
  return !$h

proc makeCanonicalCollection*[T](g: Grammar[T]): SetOfLRItems[T] =
  let init = g.closure([LRItem[T](rule: g.startRule, pos: 0)].toSet)
  result = [
    init
  ].toOrderedSet
  var checkSet = result
  while checkSet.len > 0:
    var new: SetOfLRItems[T]
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
  when defined(nimydebug):
    echo ag.followTable
    echo canonicalCollection
    echo result
