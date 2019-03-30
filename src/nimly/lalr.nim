import tables
import sets
import hashes

import patty

import parsetypes
import parser

type
  LALRItem*[T] = object
    rule: Rule[T]
    pos: int
    ahead: Symbol[T]
  LALRItems[T] = HashSet[LALRItem[T]]
  SetOfLALRItems[T] = OrderedSet[LALRItems[T]]

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
              let n = LALRITem[T](rule: r, pos: 0, ahead: fst)
              if not result.containsOrIncl(n):
                new.incl(n)
        _:
          discard
    checkSet = new

proc toLALRKernel*[T]() =
  discard

proc makeTableLALR*[T](g: Grammar[T]): ParsingTable[T] =
  discard
