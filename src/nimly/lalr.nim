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

proc fromNextNext*[T](i: LALRItem[T]): seq[Symbol[T]] =
  doAssert i.pos < i.rule.len
  result = @[]
  for index in (i.pos + 1)..<i.rule.len:
    result.add(i.rule.right[index])

proc closure[T](g: Grammar[T], single: LALRItem[T]): LALRItems[T] =
  result = g.closure([single].toSet)

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

proc toLALRKernel[T](g: Grammar[T], lrKernel: SetOfLRItems[T],
                     tt: TransTable[T]): SetOfLALRItems[T] =
  # init result
  result = initSetOfLALRItems[T]()
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
    for itm in itms:
      propagation[itm] = initSet[(int, LRItem[T])]()

      let clsr = g.closure(itm.toLALRItem(Dummy[T]()))
      for ci in clsr:
        if ci.ahead == Dummy[T]():
          if ci.next != End[T]():
            propagation[itm].incl((tt[idx][ci.next], ci.foward.toLRItem))
        else:
          let prpgtd = ci.foward
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
  discard
