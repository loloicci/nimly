import tables
import patty

import lextypes


proc `~`[A](a: A): ref A =
  new(result)
  result[] = a


type
  # for SynTree
  Pos = int16
  BOp = enum
    bor,
    bcat
  Pos2PosSet = TableRef[Pos, set[Pos]]

  # for DFA
  DState = int16
  DTranRow = TableRef[char, DState]
  DTran = TableRef[DState, DTranRow]
  DFA = object
    start: DState
    accepts: set[DState]
    states: set[DState]
    tran: DTran

variant LChar:
  End
  Real(c: char)

variant Lit:
  Empty
  Char(pos: Pos, c: LChar)

variant ReSynTree:
  Term(lit: Lit)
  Bin(op: BOp, left: ref ReSynTree, right: ref ReSynTree)
  Star(child: ref ReSynTree)

proc newPos2PosSet(): Pos2PosSet =
  result = newTable[Pos, set[Pos]]()

proc newDTran(): DTran =
  result = newTable[DState, DTranRow]()

proc newDTranRow(): DTranRow =
  result = newTable[char, DState]()

proc collectPos(t: ReSynTree): set[Pos] =
  # error on tree with not unique positions when debug
  match t:
    Term(lit: l):
      match l:
        Empty:
          return {}
        Char(pos: p, c: _):
          return {p}
    Bin(op: _, left: l, right: r):
      let
        lr = l[].collectPos
        rr = r[].collectPos
      assert lr * rr == {}
      return lr + rr
    Star(child: c):
      return c[].collectPos

proc collectChar(t: ReSynTree): set[char] =
  match t:
    Term(lit: l):
      match l:
        Empty:
          return {}
        Char(pos: p, c: lc):
          match lc:
            End:
              return {}
            Real(c: c):
              return {c}
    Bin(op: _, left: l, right: r):
      return l[].collectChar + r[].collectChar
    Star(child: c):
      return c[].collectChar

proc nullable(t: ReSynTree): bool =
  match t:
    Term(lit: l):
      match l:
        Empty:
          return true
        Char:
          return false
    Bin(op: o, left: l, right: r):
      case o
      of bor:
        return l[].nullable or r[].nullable
      of bcat:
        return l[].nullable and r[].nullable
    Star:
      return true

proc firstpos(t: ReSynTree): set[Pos] =
  match t:
    Term(lit: l):
      match l:
        Empty:
          return {}
        Char(pos: p, c: _):
          return {p}
    Bin(op: o, left: l, right: r):
      case o
      of bor:
        return l[].firstpos + r[].firstpos
      of bcat:
        if l[].nullable:
          return l[].firstpos + r[].firstpos
        else:
          return l[].firstpos
    Star(child: c):
      return c[].firstpos

proc lastpos(t: ReSynTree): set[Pos] =
  match t:
    Term(lit: l):
      match l:
        Empty:
          return {}
        Char(pos: p, c: _):
          return {p}
    Bin(op: o, left: l, right: r):
      case o
      of bor:
        return l[].lastpos + r[].lastpos
      of bcat:
        if r[].nullable:
          return l[].lastpos + r[].lastpos
        else:
          return r[].lastpos
    Star(child: c):
      return c[].lastpos

proc mergeSetTable[A; B](a: var TableRef[A, set[B]], b: TableRef[A, set[B]]) =
  for k in b.keys:
    a[k] = a.getOrDefault(k) + b[k]

proc makeFollowposTable(t: ReSynTree): Pos2PosSet =
  # init
  result = newPos2PosSet()

  # make
  match t:
    Term:
      return
    Bin(op: o, left: l, right: r):
      if o == bcat:
        for i in l[].lastpos:
          result[i] = r[].firstpos
      result.mergeSetTable(l[].makeFollowposTable)
      result.mergeSetTable(r[].makeFollowposTable)
    Star(child: c):
      for i in t.lastpos:
        result[i] = t.firstpos
      result.mergeSetTable(c[].makeFollowposTable)

proc terms(t: ReSynTree): seq[Lit] =
  match t:
    Term(lit: l):
      return @[l]
    Bin(op: _, left: l, right: r):
      return l[].terms & r[].terms
    Star(child: c):
      return c[].terms

proc makePosCharTable(t: ReSynTree): TableRef[Pos, char] =
  result = newTable[Pos, char]()

  for l in t.terms:
    match l:
      Char(pos: p, c: l):
        match l:
          Real(c: c):
            result[p] = c
          End:
            continue
      Empty:
        continue

proc makeCharPossetTable(t: ReSynTree): TableRef[char, set[Pos]] =
  # init
  let
    chars = t.collectChar

  result = newTable[char, set[Pos]]()
  for c in chars:
    result[c] = {}

  for l in t.terms:
    match l:
      Char(pos: p, c: l):
        match l:
          Real(c: c):
            result[c].incl(p)
          End:
            continue
      Empty:
        continue

proc getAccPos(t: ReSynTree): Pos =
  let chars = t.collectChar
  for l in t.terms:
    match l:
      Char(pos: p, c: l):
        match l:
          Real:
            continue
          End:
            return p
      Empty:
        continue
  assert false

proc stateNum(d: DFA): int =
  return d.states.card

proc makeDFA(t: ReSynTree): DFA =
  let
    followpos = t.makeFollowposTable

  var
    tran = newDTran()
    states: set[DState] = {}
    posS2DState = newTable[set[Pos], DSTate]()
    unmarked: seq[set[Pos]] = @[]

  # init
  let
    chars = t.collectChar
    iState = DState(states.card)
    iSPos = t.firstpos
    charPosset = t.makeCharPossetTable
  states.incl(iState)
  posS2DState[iSPos] = iState
  unmarked.add(iSPos)

  # make state and tran
  while unmarked.len > 0:
    let
      ps = unmarked.pop
      s = posS2DState[ps]
    tran[s] = newDTranRow()
    for c in chars:
      let posSet = ps * charPosset[c]
      var newSPos: set[Pos] = {}
      for p in posSet:
        newSPos = newSPos + followpos[p]
      var nState: DState
      if posS2DState.hasKey(newSPos):
        nState = posS2DState[newSPos]
      else:
        nState = DState(states.card)
        unmarked.add(newSPos)
        states.incl(nState)
        posS2DState[newSPos] = nState
      tran[s][c] = nState

  # make accepts
  let acc = t.getAccPos
  var accepts: set[DState] = {}
  for k in posS2DState.keys:
    if acc in k:
      accepts.incl(posS2DState[k])

  # make DFA
  return DFA(start: iState, accepts: accepts, states: states, tran: tran)

#[
iterator pairElm[A](s: set[A]): (A, A) =
  var tmp = s
  for e1 in s:
    tmp.excl(e2)
    for e2 in tmp:
      yield (e1, e2)
]#

proc statePartTran(state: DState, parts: seq[set[DState]],
                   dfa: DFA): TableRef[char, DState] =
  result = newTable[char, DState]()
  for c, s in dfa.tran[state]:
    for i, p in parts:
      if s in p:
        result[c] = DState(i)
        break

proc grind(parts: seq[set[DState]], dfa: DFA): seq[set[DState]] =
  result = @[]
  for setOfStates in parts:
    var subparts: seq[(set[DState], TableRef[char, DState])] = @[]
    for state in setOfStates:
      let sTran = state.statePartTran(parts, dfa)
      var isNewPart = true
      for i, sp in subparts:
        let (sos, tran) = sp
        if sTran == tran:
          subparts[i] = (sos + {state}, tran)
          isNewPart = false
          break
      if isNewPart:
        subparts.add(({state}, sTran))

    # add seq of state set to result
    for sp in subparts:
      result.add(sp[0])


proc minimizeStates(input: DFA): DFA =
  var partition: seq[set[DState]] = @[]
  var newPartition: seq[set[DState]] = @[input.states - input.accepts,
                                         input.accepts]
  while partition != newPartition:
    partition = newPartition
    newPartition = partition.grind(input)

  result = DFA(tran: newDTran())
  for i, p in partition:
    if input.start in p:
      result.start = DState(i)
    for acc in input.accepts:
      if acc in p:
        result.accepts.incl(DState(i))
    result.states.incl(DState(i))
    for s in p:
      result.tran[DState(i)] = s.statePartTran(partition, input)
      break
