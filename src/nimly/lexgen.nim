import tables
import sets
import patty

include lextypes


proc `~`[T](obj: T): ref T =
  new(result)
  result[] = obj


type
  # for SynTree
  Pos = int
  BOp = enum
    bor,
    bcat
  Pos2PosSet = TableRef[Pos, HashSet[Pos]]

  # for DFA
  DState = int
  DTranRow = TableRef[char, DState]
  DTran = TableRef[DState, DTranRow]
  DAccepts[T] = TableRef[DState, proc(token: LToken): T {.nimcall.}]
  DFA[T] = object
    start: DState
    accepts: DAccepts[T]
    stateNum: int
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

type
  AccPosProc[T] = TableRef[Pos, proc(token: LToken): T {.nimcall.}]
  LexRe[T] = object
    st: ReSynTree
    accPosProc: AccPosProc[T]

proc newPos2PosSet(): Pos2PosSet =
  result = newTable[Pos, HashSet[Pos]]()

proc newDAccepts[T](): DAccepts[T] =
  result = newTable[DState, proc(token: LToken): T {.nimcall.}]()

proc newDTran(): DTran =
  result = newTable[DState, DTranRow]()

proc newDTranRow(): DTranRow =
  result = newTable[char, DState]()

proc collectPos(t: ReSynTree): HashSet[Pos] =
  # error on tree with not unique positions when debug
  result.init
  match t:
    Term(lit: l):
      match l:
        Empty:
          return
        Char(pos: p, c: _):
          result.incl(p)
          return
    Bin(op: _, left: l, right: r):
      let
        lr = l[].collectPos
        rr = r[].collectPos
      assert (lr * rr).len == 0
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
              return
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

proc firstpos(t: ReSynTree): HashSet[Pos] =
  result.init
  match t:
    Term(lit: l):
      match l:
        Empty:
          return
        Char(pos: p, c: _):
          result.incl(p)
          return
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

proc lastpos(t: ReSynTree): HashSet[Pos] =
  result.init
  match t:
    Term(lit: l):
      match l:
        Empty:
          return
        Char(pos: p, c: _):
          result.incl(p)
          return
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

proc mergeSetTable[A; B](a: var TableRef[A, HashSet[B]],
                         b: TableRef[A, HashSet[B]]) =
  for k in b.keys:
    a[k] = a.getOrDefault(k, initSet[B]()) + b[k]

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

proc makeCharPossetTable(t: ReSynTree): TableRef[char, HashSet[Pos]] =
  # init
  let
    chars = t.collectChar

  result = newTable[char, HashSet[Pos]]()
  for c in chars:
    result[c] = initSet[Pos]()

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

proc makeDFA[T](lr: LexRe[T]): DFA[T] =
  let
    t = lr.st
    followpos = t.makeFollowposTable

  var
    tran = newDTran()
    stateNum = 0
    posS2DState = newTable[HashSet[Pos], DSTate]()
    unmarked: seq[HashSet[Pos]] = @[]

  # init
  let
    chars = t.collectChar
    iState = stateNum
    iSPos = t.firstpos
    charPosset = t.makeCharPossetTable
  inc(stateNum)
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
      var newSPos: HashSet[Pos] = initSet[Pos]()
      for p in posSet:
        newSPos = newSPos + followpos[p]
      var nState: DState
      if posS2DState.hasKey(newSPos):
        nState = posS2DState[newSPos]
      else:
        nState = stateNum
        inc(stateNum)
        unmarked.add(newSPos)
        posS2DState[newSPos] = nState
      tran[s][c] = nState

  # make accepts
  var accepts = newDAccepts[T]()
  for k in posS2DState.keys:
    for p in k:
      if lr.accPosProc.haskey(p):
        accepts[posS2DState[k]] = (lr.accPosProc[p])

  # make DFA
  return DFA[T](start: iState, accepts: accepts, stateNum: stateNum, tran: tran)

proc statePartTran[T](state: DState, parts: seq[HashSet[DState]],
                      dfa: DFA[T]): TableRef[char, DState] =
  result = newTable[char, DState]()
  for c, s in dfa.tran[state]:
    for i, p in parts:
      if s in p:
        result[c] = DState(i)
        break

proc grind[T](parts: var seq[HashSet[DState]], dfa: DFA[T]): bool =
  ## return true if grind effects
  result = false
  var retParts: seq[HashSet[DState]] = @[]
  for setOfStates in parts:
    var subparts: seq[(HashSet[DState], TableRef[char, DState])] = @[]
    for state in setOfStates:
      let sTran = state.statePartTran(parts, dfa)
      var isNewPart = true
      for i, sp in subparts:
        let (sos, tran) = sp
        if sTran == tran:
          var single = initSet[DState]()
          single.incl(state)
          subparts[i] = (sos + single, tran)
          isNewPart = false
          break
      if isNewPart:
        var single = initSet[DState]()
        single.incl(state)
        subparts.add((single, sTran))

    # add seq of state set to renew parts
    for sp in subparts:
      retParts.add(sp[0])
    if subparts.len > 1:
      result = true
  parts = retParts

proc minimizeStates[T](input: DFA[T], initPart: seq[HashSet[DState]]): DFA[T] =
  var
    partition = initPart
    didChange = true
  while didChange:
    didChange = partition.grind(input)

  result = DFA[T](tran: newDTran(), accepts: newDAccepts[T]())
  for i, p in partition:
    if input.start in p:
      result.start = i
    for acc in input.accepts.keys:
      if acc in p:
        result.accepts[i] = input.accepts[acc]
    inc(result.stateNum)
    for s in p:
      result.tran[i] = s.statePartTran(partition, input)
      break

proc defaultAndOther(dtr: DTranRow): (DState, DTranRow) =
  var counts = newTable[DState, int]()
  for s in dtr.values:
    if not counts.hasKey(s):
      counts[s] = 0
    inc(counts[s])

  var
    default: DState
    maxCnt: int = -1
  for s, c in counts:
    if maxCnt < c:
      default = s
      maxCnt = c

  var resultTranRow = newDTranRow()

  for c, s in dtr:
    if s != default:
      resultTranRow[c] = s

  return (default, resultTranRow)

proc longestEmpty(nc: seq[NC]): (int, int) =
  ## return (start, length)
  var
    start = 0
    length = 0
    maxStart = 0
    maxLen = 0
    isReadingEmpty = false

  # nc & @[DummyRow]
  for i, r in nc & @[DataRow(0, 0)]:
    # nothing
    match r:
      DataRow:
        if isReadingEmpty:
          inc(length)
        else:
          start = i
          length = 1
          isReadingEmpty = true
      EmptyRow:
        if isReadingEmpty:
          if maxLen < length:
            maxStart = start
            maxLen = length
            isReadingEmpty = false

    return (maxStart, maxLen)

proc minMaxCharIntOfRow(dtr: DTranRow): (int, int) =
  ## return (min, max)
  var
    resultMin = 256
    resultMax = -1
  for c in dtr.keys:
    resultMin = min(resultMin, int(c))
    resultMax = max(resultMax, int(c))
  assert resultMin < 256
  assert resultMax > -1

  return (resultMin, resultMax)

proc writeRow(ncTable: var NCTable, index: int, nc: NC) =
  if ncTable.len <= index:
    for i in index..<ncTable.len:
      ncTable.add(EmptyRow())
    assert ncTable.len == index, "(" & $ncTable.len & " != " & $index & ")"
    ncTable.add(nc)
  else:
    assert ncTable[index] == EmptyRow(), "Try to rewrite"
    ncTable[index] = nc

proc convertToLexData[T](dfa: DFA[T]): LexData[T] =
  var
    # first element is starting state
    dbaTable: DBATable[T] = @[DBA[T]()]
    ncTable: NCTable = @[]
  for s, tr in dfa.tran:
    let
      (default, newTranRow) = tr.defaultAndOther
      (ls, ll) = ncTable.longestEmpty
      (minC, maxC) = newTranRow.minMaxCharIntOfRow
    var
      start: int
      base: int
    if maxC - minC >= ll:
      start = ncTable.len
    else:
      start = ls
    base = start - minC

    for c, next in newTranRow:
      ncTable.writeRow(base + int(c), DataRow(next=next, check=s))

    var acc: Accept[T]
    if dfa.accepts.haskey(s):
      acc = Acc[T](dfa.accepts[s])
    else:
      acc = NotAcc[T]()

    let dba = DBA[T](default: default, base: base, accept: acc)
    if s == dfa.start:
      dbaTable[0] = dba
    else:
      dbaTable.add(dba)

  return LexData[T](dba: dbaTable, nc: ncTable)

proc nextState[T](ld: LexData[T], s: State, a: char): State =
  assert ld.dba.len > s, "(" & $ld.dba.len & " !> " & $s & ")"
  assert s > -1, "(" & $s & " !> " & "-1)"
  let
    base = ld.dba[s].base
    default = ld.dba[s].default
    index = base + int(a)
  if ld.nc.len <= index or index < 0:
    return default
  let nc = ld.nc[index]
  match nc:
    EmptyRow:
      assert false
    DataRow(n, c):
      if c == s:
        return n
      else:
        return default
