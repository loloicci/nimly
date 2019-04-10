import tables
import sets
import hashes

import patty

import parsetypes
import parser
import lr

type
  LALRItem*[T] = object
    rule: Rule[T]
    pos: int
    ahead: Symbol[T]
  LALRItems[T] = HashSet[LALRItem[T]]
  SetOfLALRItems[T] = OrderedTable[int, LALRItems[T]]
  PropagateTable[T] = Table[LRItem[T], HashSet[(int, LRItem[T])]]

proc initLALRItems[T](): LALRItems[T] =
  result = initSet[LALRItem[T]]()

proc initSetOfLALRItems[T](): SetOfLALRItems[T] =
  result = initOrderedTable[int, LALRItems[T]]()

proc initPropagateTable[T](): PropagateTable[T] =
  result = initTable[LRItem[T], HashSet[(int, LRItem[T])]]()

proc hash*[T](x: LALRItem[T]): Hash =
  var h: Hash = 0
  h = h !& hash(x.rule)
  h = h !& hash(x.pos)
  h = h !& hash(x.ahead)
  return !$h

proc next*[T](i: LALRItem[T]): Symbol[T] =
  if i.pos >= i.rule.len:
    return End[T]()
  result = i.rule.right[i.pos]

proc nextSkipEmpty*[T](i: LALRItem[T]): Symbol[T] =
  result = End[T]()
  for idx in i.pos..<i.rule.len:
    let nxt = i.rule.right[idx]
    if nxt != Empty[T]():
      result = nxt
      break

proc fromNextNext*[T](i: LALRItem[T]): seq[Symbol[T]] =
  result = @[]
  doAssert i.pos < i.rule.len
  for index in (i.pos + 1)..<i.rule.len:
    result.add(i.rule.right[index])

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
            for fst in g.calFirsts(i.fromNextNext & i.ahead):
              let n = LALRItem[T](rule: r, pos: 0, ahead: fst)
              if not result.containsOrIncl(n):
                new.incl(n)
        _:
          discard
    checkSet = new

proc closure[T](g: Grammar[T], single: LALRItem[T]): LALRItems[T] =
  result = g.closure([single].toSet)

proc toLALRItem[T](lrItem: LRItem[T], ahead: Symbol[T]): LALRItem[T] =
  result = LALRItem[T](rule: lrItem.rule, pos: lrItem.pos, ahead: ahead)

proc toLRItem[T](lalrItem: LALRItem[T]): LRItem[T] =
  result = LRItem[T](rule: lalrItem.rule, pos: lalrItem.pos)

proc `[]`[T](pt: PropagateTable[T],
             itm: LALRItem[T]): HashSet[(int, LRItem[T])] =
  result = pt[LRItem[T](rule: itm.rule, pos: itm.pos)]

proc incl[T](ot: var OrderedTable[int, T], vl: T) =
  ot[ot.len] = vl

proc foward[T](itm: LALRItem[T]): LALRItem[T] =
  result = LALRItem[T](rule: itm.rule, pos: itm.pos + 1, ahead: itm.ahead)

proc toLALRKernel[T](lrKernel: SetOfLRItems[T], g: Grammar[T],
                     tt: TransTable[T]): SetOfLALRItems[T] =
  # init result
  result = initSetOfLALRItems[T]()
  doAssert lrKernel.card > 0
  for idx in 0..<lrKernel.card:
    result.incl(initLALRItems[T]())
  var
    propagation: PropagateTable[T] = initPropagateTable[T]()
    checkSet: HashSet[LALRItem[T]] = initLALRItems[T]()

  # only starting rule
  for sk in lrKernel:
    for si in sk:
      result[0].incl(si.toLALRItem(End[T]()))
      checkSet.incl(si.toLALRItem(End[T]()))
      break
    break

  # init collection and cal propagate
  for idx, itms in lrKernel:
    echo "[nimly] converting kernel: " & $(idx + 1) & "/" & $lrKernel.len
    for itm in itms:
      if not (propagation.haskey(itm)):
        propagation[itm] = initSet[(int, LRItem[T])]()

      let clsr = g.closure(itm.toLALRItem(Dummy[T]()))
      for ci in clsr:
        if ci.ahead == Dummy[T]():
          if ci.next != End[T]():
            propagation[itm] = (propagation[itm] +
                                [(tt[idx][ci.next],
                                  ci.foward.toLRItem)].toSet)
        else:
          let prpgtd = ci.foward
          assert tt[idx][ci.next] < lrKernel.card
          result[tt[idx][ci.next]].incl(prpgtd)
          checkSet.incl(prpgtd)

  # cal collection
  while checkSet.card > 0:
    var newSet = initLALRItems[T]()
    for itm in checkSet:
      for toInfo in propagation[itm]:
        let
          (idx, toItm) = toInfo
          new = toItm.toLALRItem(itm.ahead)
        if not (result[idx].containsOrIncl(new)):
          newSet.incl(new)
    checkSet = newSet

proc makeTableLALR*[T](g: Grammar[T]): ParsingTable[T] =
  var
    actionTable: ActionTable[T]
    gotoTable: GotoTable[T]
  actionTable = initTable[State, ActionRow[T]]()
  gotoTable = initTable[State, GotoRow[T]]()
  echo "[nimly] start: make table for parser"
  let
    ag = if g.isAugument:
           g
         else:
           g.augument
    (cc, tt) = makeCanonicalCollection[T](ag)
    knl = cc.filterKernel
    lalrKnl = knl.toLALRKernel(ag, tt)
  echo "[nimly] done: make lalrkernel"
  for idx, itms in lalrKnl:
    echo "[nimly] processing: Collection " & $(idx + 1) & "/" & $lalrKnl.len
    actionTable[idx] = initTable[Symbol[T], ActionTableItem[T]]()
    gotoTable[idx] = initTable[Symbol[T], State]()
    echo "[nimly] processing: Collection " & $(idx + 1) & " - make closure"
    let clsr = ag.closure(itms)
    var cnt = 1
    for itm in clsr:
      echo "[nimly] processing: Collection " & $(idx + 1) & " - " &
        $cnt & "/" & $clsr.card
      inc(cnt)
      let sym = itm.nextSkipEmpty
      match sym:
        TermS:
          when defined(nimydebug):
            if actionTable[idx].haskey(sym) and
               actionTable[idx][sym].kind == ActionTableItemKind.Reduce:
              echo "LALR:CONFLICT!!!" & $idx & ":" & $sym
          actionTable[idx][sym] = Shift[T](tt[idx][sym])
        NonTermS:
          gotoTable[idx][sym] = tt[idx][sym]
        End:
          if itm.rule.left == ag.start:
            actionTable[idx][End[T]()] = Accept[T]()
          else:
            if actionTable[idx].haskey(itm.ahead) and
               actionTable[idx][itm.ahead].kind == ActionTableItemKind.Shift:
              when defined(nimydebug):
                echo "LALR:CONFLICT!!!" & $idx & ":" & $itm.ahead
              continue
            actionTable[idx][itm.ahead] = Reduce[T](itm.rule)
        _:
          discard
  echo "[nimly] done: make tables"
  result = ParsingTable[T](action: actionTable, goto: gotoTable)
  when defined(nimydebug):
    echo "LALR:"
    echo result
