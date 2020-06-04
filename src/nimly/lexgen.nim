import tables
import sets
import strutils
import macros
import patty

import lextypes

export tables
export sets

proc `~`*[T](obj: T): ref T =
  new(result)
  result[] = obj

type
  # for SynTree
  Pos* = int
  BOp* = enum
    bor,
    bcat
  Pos2PosSet = TableRef[Pos, HashSet[Pos]]

  # for DFA
  DState = int
  DTranslationsRow = TableRef[char, DState]
  DTranslations = TableRef[DState, DTranslationsRow]
  DAccepts[T] = TableRef[DState, AccProc[T]]
  DFA[T] = object
    start: DState
    accepts: DAccepts[T]
    stateNum: int
    translations: DTranslations

variant LChar:
  End
  Real(c: char)

variant Lit:
  Empty
  Char(pos: Pos, c: LChar)

variantp ReSynTree:
  Term(lit: Lit)
  Bin(op: BOp, left: ref ReSynTree, right: ref ReSynTree)
  Star(child: ref ReSynTree)

type
  AccPosProc[T] = TableRef[Pos, AccProc[T]]
  LexRe*[T] = object
    st: ReSynTree
    accPosProc: AccPosProc[T]

proc newAccPosProc*[T](): AccPosProc[T] =
  result = newTable[Pos, AccProc[T]]()

proc newPos2PosSet(): Pos2PosSet =
  result = newTable[Pos, HashSet[Pos]]()

proc newDAccepts[T](): DAccepts[T] =
  result = newTable[DState, AccProc[T]]()

proc newDTranslations(): DTranslations =
  result = newTable[DState, DTranslationsRow]()

proc newDTranslationsRow(): DTranslationsRow =
  result = newTable[char, DState]()

proc accPosImplDebug(t: ReSynTree): seq[Pos] {.used.} =
  result = @[]
  var checkSet: seq[ReSynTree] = @[t]
  while checkSet.len > 0:
    let ct = checkSet.pop
    match ct:
      Term(lit: l):
        match l:
          Empty:
            continue
          Char(pos: p, c: c):
            if c.kind == LCharKind.End:
              result &= p
            else:
              continue
      Star(child: c):
        checkSet.add(c[])
      Bin(op:_, left: l, right: r):
        checkSet.add(r[])
        checkSet.add(l[])

proc accPosImpl(t: ReSynTree): seq[Pos] {.used.} =
  result = @[]
  var checkSet: seq[ReSynTree] = @[t]
  while checkSet.len > 0:
    let ct = checkSet.pop
    match ct:
      Term(lit: l):
        match l:
          Empty:
            return @[]
          Char(pos: p, c: c):
            if c.kind == LCharKind.End:
              return @[p]
            else:
              return @[]
      Star(child: c):
        checkSet = @[c[]] & checkSet
      Bin(op:_, left: l, right: r):
        checkSet = @[r[]] & checkSet
        checkSet = @[l[]] & checkSet

when not defined(release):
  import sequtils

proc accPos*(t: ReSynTree): seq[Pos] =
  ## use it if acc is only one position
  result = @[]
  when defined(release):
    result = t.accPosImpl
  else:
    result = t.accPosImplDebug.deduplicate
    doassert result.len > 0, "No acc node"

proc reassignPos(t: ReSynTree, nextPos: var int): ReSynTree =
  match t:
    Term(lit: l):
      match l:
        Empty:
          return Term(Empty())
        Char(pos: _, c: c):
          result = Term(Char(pos = nextPos, c = c))
          inc(nextPos)
          return
    Bin(op: op, left: l, right: r):
      let
        left = ~l[].reassignPos(nextPos)
        right = ~r[].reassignPos(nextPos)
      return Bin(op = op,
                 left = left,
                 right = right)
    Star(child: c):
      return Star(child = ~c[].reassignPos(nextPos))

proc collectChar(t: ReSynTree): set[char] =
  result = {}
  var checkSet: seq[ReSynTree] = @[t]
  while checkSet.len > 0:
    let ct = checkSet.pop
    match ct:
      Term(lit: l):
        match l:
          Empty:
            continue
          Char(pos: _, c: lc):
            match lc:
              End:
                continue
              Real(c: c):
                result.incl(c)
      Bin(op: _, left: l, right: r):
        checkSet.add(l[])
        checkSet.add(r[])
      Star(child: c):
        checkSet.add(c[])

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
  var checkSet: seq[ReSynTree] = @[t]
  while checkSet.len > 0:
    let ct = checkSet.pop
    match ct:
      Term(lit: l):
        match l:
          Empty:
            continue
          Char(pos: p, c: _):
            result.incl(p)
            continue
      Bin(op: o, left: l, right: r):
        case o
        of bor:
          checkSet.add(l[])
          checkSet.add(r[])
        of bcat:
          checkSet.add(l[])
          if l[].nullable:
            checkSet.add(r[])
      Star(child: c):
        checkSet.add(c[])

proc lastpos(t: ReSynTree): HashSet[Pos] =
  result.init
  var checkSet: seq[ReSynTree] = @[t]
  while checkSet.len > 0:
    let ct = checkSet.pop
    match ct:
      Term(lit: l):
        match l:
          Empty:
            continue
          Char(pos: p, c: _):
            result.incl(p)
            continue
      Bin(op: o, left: l, right: r):
        case o
        of bor:
          checkSet.add(l[])
          checkSet.add(r[])
        of bcat:
          if r[].nullable:
            checkSet.add(l[])
          checkSet.add(r[])
      Star(child: c):
        checkSet.add(c[])

proc makeFollowposTable(t: ReSynTree): Pos2PosSet =
  # init
  result = newPos2PosSet()
  var checkSet: seq[ReSynTree] = @[t]

  while checkSet.len > 0:
    let ct = checkSet.pop
    # make
    match ct:
      Term:
        continue
      Bin(op: o, left: l, right: r):
        if o == bcat:
          for i in l[].lastpos:
            if result.hasKey(i):
              result[i] = result[i] + r[].firstpos
            else:
              result[i] = r[].firstpos
        checkSet.add(l[])
        checkSet.add(r[])
      Star(child: c):
        for i in ct.lastpos:
          if result.haskey(i):
            result[i] = result[i] + ct.firstpos
          else:
            result[i] = ct.firstpos
        checkSet.add(c[])

proc terms(t: ReSynTree): seq[Lit] =
  result = @[]
  var checkSet: seq[ReSynTree] = @[t]
  while checkSet.len > 0:
    let ct = checkSet.pop
    match ct:
      Term(lit: l):
        result.add(l)
      Bin(op: _, left: l, right: r):
        checkSet.add(l[])
        checkSet.add(r[])
      Star(child: c):
        checkSet.add(c[])

proc makeCharPossetTable(t: ReSynTree): TableRef[char, HashSet[Pos]] =
  # init
  let
    chars = t.collectChar

  result = newTable[char, HashSet[Pos]]()
  for c in chars:
    result[c] = initHashSet[Pos]()

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

proc makeDFA*[T](lr: LexRe[T]): DFA[T] =
  when defined(nimydebug):
    echo "[nimly] start : make DFA"

  let
    t = lr.st
    followpos = t.makeFollowposTable

  var
    translations = newDTranslations()
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

  # make state and translations
  while unmarked.len > 0:
    let
      ps = unmarked.pop
      s = posS2DState[ps]
    translations[s] = newDTranslationsRow()
    for c in chars:
      let posSet = ps * charPosset[c]
      var newSPos: HashSet[Pos] = initHashSet[Pos]()
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
      translations[s][c] = nState

  # make accepts
  var accepts = newDAccepts[T]()
  for k in posS2DState.keys:
    # the first acc position is expected acc
    var mp = high(int)
    for p in k:
      if lr.accPosProc.haskey(p):
        if p < mp:
          mp = p
    if mp != high(int):
      accepts[posS2DState[k]] = (lr.accPosProc[mp])

  when defined(nimydebug):
    echo "[nimly] done : make DFA"
  # make DFA
  return DFA[T](start: iState, accepts: accepts,
                stateNum: stateNum, translations: translations)

proc calculateTableCharsToNextPartitions[T](
  state: DState,
  partitions: seq[HashSet[DState]],
  dfa: DFA[T]): TableRef[char, DState] =
  result = newTable[char, DState]()
  for c, s in dfa.translations[state]:
    for i, p in partitions:
      if s in p:
        result[c] = DState(i)
        break

proc grind[T](parts: var seq[HashSet[DState]], dfa: DFA[T]): bool =
  ## return true if this affects `parts`
  result = false
  var retParts: seq[HashSet[DState]] = @[]
  for setOfStates in parts:
    var subparts: seq[(HashSet[DState], TableRef[char, DState])] = @[]
    for state in setOfStates:
      let sTranslations = state.calculateTableCharsToNextPartitions(parts, dfa)
      var isNewPart = true
      for i, sp in subparts:
        let (sos, translations) = sp
        if sTranslations == translations:
          var single = initHashSet[DState]()
          single.incl(state)
          subparts[i] = (sos + single, translations)
          isNewPart = false
          break
      if isNewPart:
        var single = initHashSet[DState]()
        single.incl(state)
        subparts.add((single, sTranslations))

    # add seq of state set to renew parts
    for sp in subparts:
      retParts.add(sp[0])
    if subparts.len > 1:
      result = true
  parts = retParts

proc removeDead[T](input: DFA[T]): DFA[T] =
  var dead = initHashSet[DState]()
  for s, tr in input.translations:
    if input.accepts.haskey(s):
      continue
    var f = true
    for ns in tr.values:
      if s != ns:
        f = false
        break
    if f:
      dead.incl(s)
  var newTranslations = newDTranslations()
  for s, tr in input.translations:
    if s in dead:
      continue
    var newRow = newDTranslationsRow()
    for c, ns in tr:
      if ns in dead:
        newRow[c] = deadState
      else:
        newRow[c] = ns
    newTranslations[s] = newRow
  result = DFA[T](
    start: input.start,
    accepts: input.accepts,
    stateNum: input.stateNum - dead.card,
    translations: newTranslations
  )

proc minimizeStates[T](input: DFA[T],
                       initPartition: seq[HashSet[DState]]): DFA[T] =
  ## The main part of `minimizeStates*[T](input: DFA[T]): DFA[T]`.
  ## `initPartiotion[0]` needs to be the state to accept.
  var
    partition = initPartition
    didChange = true
  while didChange:
    didChange = partition.grind(input)

  result = DFA[T](translations: newDTranslations(),
                  accepts: newDAccepts[T]())
  for i, p in partition:
    if input.start in p:
      result.start = i
    for acc in input.accepts.keys:
      if acc in p:
        result.accepts[i] = input.accepts[acc]
    inc(result.stateNum)
    for s in p:
      result.translations[i] = s.calculateTableCharsToNextPartitions(partition,
                                                                     input)
      break

  result = result.removeDead

proc minimizeStates*[T](input: DFA[T]): DFA[T] =
  ## Minimmize the state of DNF.
  ## The algorithm is same to what is explained in DragonBook 3.9.7.
  ## After despatch this function,
  ## each states to accept in DFA needs to correspond to the unique clause.
  when defined(nimydebug):
    echo "[nimly] start : minimize lexer state"
  var
    initPartition: seq[HashSet[DState]] = @[]
    other = initHashSet[DState]()
  for i in 0..<input.stateNum:
    other.incl(i)

  for k in input.accepts.keys:
    var single = initHashSet[DState]()
    single.incl(k)
    other.excl(k)
    initPartition.add(single)
  initPartition.add(other)
  when defined(nimldebug):
    echo "initPart\n--------"
    echo initPart
    echo "--------"

  # The main part
  result = input.minimizeStates(initPartition)
  when defined(nimydebug):
    echo "[nimly] done : minimize lexer state"

proc defaultAndOther(dtr: DTranslationsRow): (DState, DTranslationsRow) =
  var counts = newTable[DState, int]()
  for i in {0..255}:
    let s = dtr.getOrDefault(char(i), deadState)
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

  var resultTranslationsRow = newDTranslationsRow()

  for i in {0..255}:
    let ns = dtr.getOrDefault(char(i), deadState)
    if ns != default:
      resultTranslationsRow[char(i)] = ns

  return (default, resultTranslationsRow)

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

proc minMaxCharIntOfRow(dtr: DTranslationsRow): (int, int) =
  ## return (min, max)
  var
    resultMin = 256
    resultMax = -1
  for c in dtr.keys:
    resultMin = min(resultMin, int(c))
    resultMax = max(resultMax, int(c))
  if resultMin == 256:
    return (0, 0)
  return (resultMin, resultMax)

proc writeRow(ncTable: var NCTable, index: int, nc: NC, force = false) =
  if ncTable.len <= index:
    for i in ncTable.len..<index:
      ncTable.add(EmptyRow())
    assert ncTable.len == index, "(" & $ncTable.len & " != " & $index & ")"
    ncTable.add(nc)
  else:
    if not force:
      assert ncTable[index] == EmptyRow(), "Try to rewrite"
    ncTable[index] = nc

proc convertToLexData*[T](dfa: DFA[T]): LexData[T] =
  when defined(nimydebug):
    echo "[nimly] start : make lexer table"
  var
    # first element is starting state
    dbaTable: DBATable[T] = @[DBA[T]()]
    ncTable: NCTable = @[]
    # state -> newState, base, newTranslations
    stateTable = newTable[int, (int, int, DTranslationsRow)]()
  assert dbaTable.len == 1
  for s, tr in dfa.translations:
    let
      (default, newTranslationsRow) = tr.defaultAndOther
      (ls, ll) = ncTable.longestEmpty
      (minC, maxC) = newTranslationsRow.minMaxCharIntOfRow
    var
      start: int
      base: int
    if maxC - minC >= ll:
      start = ncTable.len
    else:
      start = ls
    base = start - minC

    for c, next in newTranslationsRow:
      # Dummy
      ncTable.writeRow(base + int(c),
                       DataRow(next = -2, check = -2))

    var acc: Accept[T]
    if dfa.accepts.haskey(s):
      acc = Acc[T](dfa.accepts[s])
    else:
      acc = NotAcc[T]()

    let dba = DBA[T](default: default, base: base, accept: acc)
    if s == dfa.start:
      stateTable[s] = (0, base, newTranslationsRow)
      dbaTable[0] = dba
    else:
      stateTable[s] = (dbaTable.len, base, newTranslationsRow)
      dbaTable.add(dba)
  for k, v in stateTable:
    for c, next in v[2]:
      ncTable.writeRow(v[1] + int(c),
                       DataRow(
                         next = stateTable[next][0],
                         check = v[0]),
                       force = true)

  when defined(nimydebug):
    echo "[nimly] done : make lexer table"
  return LexData[T](dba: dbaTable, nc: ncTable)

proc nextState*[T](ld: LexData[T], s: State, a: char): State =
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
      return default
    DataRow(n, c):
      if c == s:
        return n
      else:
        return default

proc isAcc*[T](ld: LexData[T], s: State): bool =
  if s < 0 or ld.dba.len <= s:
    return false
  return ld.dba[s].accept.kind == AcceptKind.Acc

variant RePart:
  RChar(c: char)
  Special(sc: char)
  Brace(s: int, e: int)
  Tree(tree: ReSynTree)

const
  allChars = {char(0)..char(255)}
  dChars = {'0'..'9'}
  nDChars = allChars - dChars
  sChars = {' ', '\t', '\n', '\r', '\f', '\v'}
  nSChars = allChars - sChars
  wChars = {'a'..'z', 'A'..'Z', '0'..'9', '-'}
  nWChars = allChars - wChars
  classTable = {'d': dChars, 'D': nDChars, 's': sChars, 'S': nSChars,
                 'w': wChars, 'W': nWChars, '.': allChars}.toTable()
  readingClass = int8(0)
  readingEscape = int8(1)
  readingDot = int8(2)
  # classHead = int8(3)
  readingBraceS = int8(4)
  readingBraceE = int8(5)
  readingClassRange = int8(6)
  classNegate = int8(7)


proc classIncl(class: var set[char], c: char,
               classBfr: var int, flag: var set[int8]) =
  if readingClassRange in flag:
    doassert classBfr >= 0, "invalid - or .. in class"
    class = class + {char(classBfr)..c}
    classBfr = -1
    flag.excl(readingClassRange)
  else:
    class.incl(c)
    classBfr = int(c)

proc classUnion(class: var set[char], s: set[char],
                 classBfr: var int, flag: var set[int8]) =
  assert (not (readingClassRange in flag)), "invalid - or .. in class"
  classBfr = -1
  class = class + s

proc convertToTree(input: set[char]): ReSynTree =
  var isFirst = true
  for c in input:
    if isFirst:
      result = Term(lit = Char(pos = -1, c = Real(c = c)))
      isFirst = false
    else:
      result = Bin(op = bor,
                   left = ~Term(lit = Char(pos = -1, c = Real(c = c))),
                   right = ~result)

  doassert (not isFirst), "There are some empty character class"

proc convertToSeqRePart(re: string): seq[RePart] =
  result = @[]

  var
    flag: set[int8] = {}
    class: set[char] = {}
    classBfr = -1
    braceS = ""
    braceE = ""

  for i, c in re:
    if readingClass in flag:
      if readingEscape in flag:
        case c
        of ']', '\\', '-':
          class.classIncl(c, classBfr, flag)
        of '^':
          doassert class.card == 0, "invalid escaping for ^ in class"
          class.classIncl(c, classBfr, flag)
        of 'd', 'D', 's', 'S', 'w', 'W':
          class.classUnion(classTable[c], classBfr, flag)
        else:
          assert false, "invalid escaping in class"
        flag.excl(readingEscape)
      else:
        if readingDot in flag:
          flag.excl(readingDot)
          if c == '.':
            flag.incl(readingClassRange)
            continue
          else:
            class.classIncl(c, classBfr, flag)
        case c
        of '\\':
          flag.incl(readingEscape)
        of '-':
          flag.incl(readingClassRange)
        of '.':
          flag.incl(readingDot)
        of '^':
          if class.card == 0:
            flag.incl(classNegate)
          else:
            class.classIncl(c, classBfr, flag)
        of ']':
          doassert (not (readingClassRange in flag)), "invalid - or .. in []"
          if classNegate in flag:
            class = allChars - class
          result.add(Tree(class.convertToTree))
          class = {}
          classBfr = -1
          flag.excl(classNegate)
          flag.excl(readingClass)
        else:
          class.classIncl(c, classBfr, flag)
    elif readingBraceS in flag:
      doassert c in dChars + {' ', ',', '}'}, "invalid {}"
      if c in dChars:
        braceS &= c
      elif c == ',':
        flag.excl(readingBraceS)
        flag.incl(readingBraceE)
      elif c == '}':
        result.add(Brace(s=braceS.parseInt, e=braceS.parseInt))
        braceS = ""
        braceE = ""
        flag.excl(readingBraceS)
    elif readingBraceE in flag:
      doassert c in dChars + {' ', '}'}, "invalid {}"
      if c in dChars:
        braceE &= c
      elif c == '}':
        result.add(Brace(s=braceS.parseInt, e=braceE.parseInt))
        braceS = ""
        braceE = ""
        flag.excl(readingBraceE)
    else:
      if readingEscape in flag:
        case c
        of '\\', '.', '[', '|', '(', ')', '?', '*', '+', '{':
          result.add(RChar(c=c))
        of 'd', 'D', 's', 'S', 'w', 'W':
          result.add(Tree(classTable[c].convertToTree))
        else:
          doassert false, "Invalid escaping"
        flag.excl(readingEscape)
      else:
        case c
        of '\\':
          flag.incl(readingEscape)
        of '.':
          result.add(Tree(allChars.convertToTree))
        of '[':
          assert (not (classNegate in flag))
          assert (not (readingClassRange in flag))
          assert classBfr == -1
          assert class.card == 0
          flag.incl(readingClass)
        of '{':
          flag.incl(readingBraceS)
        of '|', '(', ')', '?', '*', '+':
          result.add(Special(sc=c))
        else:
          result.add(RChar(c=c))

proc toTree(input: RePart): ReSynTree =
  match input:
    RChar(c: c):
      result = Term(lit = Char(pos = -1, c = Real(c = c)))
    Tree(tree: t):
      result = t
    Brace:
      doassert false, "Invalid Re (Brace)"
    Special(sc: sc):
      doassert false, "Invalid Re (" & $sc & ")"

proc treeQuestion(input: RePart): ReSynTree =
  result = Bin(op = bor,
               left = ~Term(lit = Empty()),
               right = ~input.toTree())

proc treeStar(input: RePart): ReSynTree =
  result = Star(child = ~input.toTree)

proc treePlus(input: RePart): ReSynTree =
  result = Bin(op = bcat,
               left = ~input.toTree,
               right = ~Star(child = ~input.toTree))

proc treeBrace(input: RePart, s, e: int): ReSynTree =
  result = input.toTree
  for i in 0..(e - s):
    result = Bin(op = bor,
                 left = ~Term(Empty()),
                 right = ~Bin(op = bcat,
                              left = ~input.toTree,
                              right = ~result))
  for i in 0..<s:
    result = Bin(op = bcat,
                 left = ~input.toTree,
                 right = ~result)

proc handleQuantifier(input: seq[RePart]): seq[RePart] =
  result = @[]
  var bfr = input[0]
  for i, rp in input[1..(input.len - 1)]:
    match rp:
      RChar(c: _):
        if bfr.kind in {RePartKind.RChar, RePartKind.Tree}:
          result.add(bfr)
      Special(sc: c):
        doassert (bfr.kind != RePartKind.Special and
                  bfr.kind != RePartKind.Brace), "invalid quantifier"
        case c
        of '?':
          result.add(Tree(tree = bfr.treeQuestion))
        of '*':
          result.add(Tree(tree = bfr.treeStar))
        of '+':
          result.add(Tree(tree = bfr.treePlus))
        of '|':
          if bfr.kind in {RePartKind.RChar, RePartKind.Tree}:
            result.add(bfr)
          result.add(rp)
        else:
          assert false, "'" & $c & "' cannot exists here"
      Brace(s: s, e: e):
        doassert (bfr.kind != RePartKind.Special and
                  bfr.kind != RePartKind.Brace), "invalid quantifier"
        result.add(Tree(tree = bfr.treeBrace(s, e)))
      Tree(tree: _):
        if bfr.kind in {RePartKind.RChar, RePartKind.Tree}:
          result.add(bfr)
    bfr = rp
  if bfr.kind in {RePartKind.RChar, RePartKind.Tree}:
    result.add(bfr)

proc handleCat(input: seq[RePart]): ReSynTree =
  var ip = input
  if ip.len > 0:
    result = ip.pop.toTree
  while ip.len > 0:
    result = Bin(op = bcat,
                 left = ~ip.pop.toTree,
                 right = ~result)

proc handleOr(input: seq[RePart]): ReSynTree =
  for i, rp in input:
    if rp.kind == RePartKind.Special and rp.sc == '|':
      return Bin(op = bor,
                 left = ~input[0..(i - 1)].handleCat,
                 right = ~input[(i + 1)..(input.len - 1)].handleOr)
  return input.handleCat

proc handleSubpattern(input: seq[RePart]): ReSynTree =
  var
    startPos = -1
  for i, rp in input:
    if rp.kind == RePartKind.Special and rp.sc == '(':
      startPos = i
    elif rp.kind == RePartKind.Special and rp.sc == ')':
      doassert startPos > -1, "Invalid end of Paren"
      return (
        input[0..<startPos] &
        Tree(input[(startPos + 1)..<i].handleQuantifier.handleOr) &
        input[(i + 1)..<input.len]
        ).handleSubpattern
  doassert startPos < 0, "Invalid start of Paren"
  return input.handleQuantifier.handleOr

proc convertToReSynTree*(re: string, nextPos: var int): ReSynTree =
  result = Bin(op = bcat,
               left = ~re.convertToSeqRePart.handleSubpattern,
               right = ~Term(lit = Char(pos = -1, c = End()))).reassignPos(
                 nextPos)

proc makeLexerMakerBody(typeId, body: NimNode): (NimNode, seq[NimNode]) =
  var
    lexerMakerBody = newStmtList()
    procs: seq[NimNode] = @[]

  lexerMakerBody.add(
    nnkWhenStmt.newTree(
      nnkElifBranch.newTree(
        nnkCall.newTree(
          newIdentNode("defined"),
          newIdentNode("nimlydebug")
        ),
        nnkStmtList.newTree(
          nnkCommand.newTree(
            newIdentNode("echo"),
            newLit("START: makeing the lexer")
          )
        )
      )
    )
  )

  # init part

  # var app = newAccPosProc[T]()
  let app = genSym(nskVar)
  lexerMakerBody.add(
    newVarStmt(
      app,
      newCall(
        newTree(
          nnkBracketExpr,
          newIdentNode("newAccPosProc"),
          typeId
        )
      )
    )
  )
  # var acc: seq[Pos]
  let acc = genSym(nskVar)
  lexerMakerBody.add(
    nnkStmtList.newTree(
      nnkVarSection.newTree(
        nnkIdentDefs.newTree(
          acc,
          nnkBracketExpr.newTree(
            newIdentNode("seq"),
            newIdentNode("Pos")
          ),
          newEmptyNode()
        )
      )
    )
  )

  # var wholeRst: ReSynTree
  let wholeRst = genSym(nskVar)
  lexerMakerBody.add(
    newTree(
      nnkVarSection,
      newTree(
        nnkIdentDefs,
        wholeRst,
        newIdentNode("ReSynTree"),
        newEmptyNode()
      )
    )
  )

  # var newPos = 0
  let newPos = genSym(nskVar)
  lexerMakerBody.add(
    newVarStmt(
      newPos,
      newIntLitNode(0))
  )

  # var rst: ReSynTree
  let rst = genSym(nskVar)
  lexerMakerBody.add(
    nnkStmtList.newTree(
      nnkVarSection.newTree(
        nnkIdentDefs.newTree(
          rst,
          newIdentNode("ReSynTree"),
          newEmptyNode()
        )
      )
    )
  )

  # clause parser
  proc parseClause(clause: NimNode, first: bool,
                   lexerMakerBody: var NimNode, procs: var seq[NimNode]) =
    clause.expectKind(nnkCall)
    clause[0].expectKind({nnkRStrLit, nnkStrLit})
    clause[1].expectKind(nnkStmtList)
    let
      param = newTree(nnkIdentDefs,
                      newIdentNode("token"),
                      newIdentNode("LToken"),
                      newEmptyNode())
    # make proc to return token
    let
      procName = genSym(nskProc)
      procNode = newProc(
        name = procName,
        params = @[typeId, param],
        body = clause[1]
      )
    procs.add(procNode)

    # rst = (meta clause[0]).convertToReSynTree(newPos)
    lexerMakerBody.add(
      newAssignment(
        rst,
        newCall(
          newIdentNode("convertToReSynTree"),
          clause[0],
          newPos
        )
      )
    )
    if first:
      # wholeRst = rst
      lexerMakerBody.add(
        newAssignment(
          wholeRst,
          rst
        )
      )
    else:
      # wholeRst = Bin(bor,
      #                ~rst,
      #                ~wholeRst)
      lexermakerBody.add(
        newAssignment(
          wholeRst,
          newCall(
            newIdentNode("Bin"),
            newIdentNode("bor"),
            nnkPrefix.newTree(
              newIdentNode("~"),
              wholeRst
            ),
            nnkPrefix.newTree(
              newIdentNode("~"),
              rst
            )
          )
        )
      )

    # acc = accPos(rst)
    lexerMakerBody.add(
      nnkStmtList.newTree(
        nnkAsgn.newTree(
          acc,
          nnkCall.newTree(
            newIdentNode("accPos"),
            rst
          )
        )
      )
    )
    # for a in acc:
    #   app[a] = (meta procName)
    lexerMakerBody.add(
      nnkStmtList.newTree(
        nnkForStmt.newTree(
          newIdentNode("a"),
          acc,
          nnkStmtList.newTree(
            nnkCall.newTree(
              nnkAccQuoted.newTree(
                newIdentNode("[]=")
              ),
              app,
              newIdentNode("a"),
              procName
            )
          )
        )
      )
    )

  if body.len < 1:
    error "no clouse in niml"

  # main part
  var first = true
  for clause in body:
    if clause.kind == nnkCommentStmt:
      continue
    clause.parseClause(first, lexerMakerBody, procs)
    first = false

    # make tail of lexerMakerBody

  # let lr = LexRe[(meta typeId)](st: wholeRst, accPosProc: app)
  let lrId = genSym()
  lexerMakerBody.add(
    nnkLetSection.newTree(
      nnkIdentDefs.newTree(
        lrId,
        newEmptyNode(),
        nnkObjConstr.newTree(
          nnkBracketExpr.newTree(
            newIdentNode("LexRe"),
            typeId
          ),
          nnkExprColonExpr.newTree(
            newIdentNode("st"),
            wholeRst
          ),
          nnkExprColonExpr.newTree(
            newIdentNode("accPosProc"),
            app
          )
        )
      )
    )
  )

  # let dfa = makeDFA[(meta typeId)](lr)
  let dfaId = genSym()
  lexerMakerBody.add(
    nnkLetSection.newTree(
      nnkIdentDefs.newTree(
        dfaId,
        newEmptyNode(),
        nnkCall.newTree(
          nnkBracketExpr.newTree(
            newIdentNode("makeDFA"),
            typeId
          ),
          lrId
        )
      )
    )
  )

  # let minimizedDfa = minimizeStates(dfa)
  let minimizedDfa = genSym()
  when defined(nimlnonmin):
    lexerMakerBody.add(
      nnkStmtList.newTree(
        nnkLetSection.newTree(
          nnkIdentDefs.newTree(
            minimizedDfa,
            newEmptyNode(),
            dfaId
          )
        )
      )
    )

  else:
    lexerMakerBody.add(
      nnkStmtList.newTree(
        nnkLetSection.newTree(
          nnkIdentDefs.newTree(
            minimizedDfa,
            newEmptyNode(),
            nnkCall.newTree(
              newIdentNode("minimizeStates"),
              dfaId
            )
          )
        )
      )
    )

  # result = convertToLexData(dfa)
  lexerMakerBody.add(
    nnkStmtList.newTree(
      nnkAsgn.newTree(
        newIdentNode("result"),
        nnkCall.newTree(
          newIdentNode("convertToLexData"),
          minimizedDfa
        )
      )
    )
  )

  lexerMakerBody.add(
    nnkWhenStmt.newTree(
      nnkElifBranch.newTree(
        nnkCall.newTree(
          newIdentNode("defined"),
          newIdentNode("nimlydebug")
        ),
        nnkStmtList.newTree(
          nnkCommand.newTree(
            newIdentNode("echo"),
            newLit("END: makeing the lexer")
          )
        )
      )
    )
  )

  return (lexerMakerBody, procs)

macro niml*(name, body: untyped): untyped =
  name.expectKind(nnkBracketExpr)
  body.expectKind(nnkStmtList)
  let
    nameStr = name[0].strVal
    typeId = name[1]
  result = newStmtList()

  # make lexerMakerBody
  let (lexerMakerBody, procs) = makeLexerMakerBody(typeId, body)

  # add procs to result
  result.add(procs)

  # define proc lexerMaker in result

  # proc ...Maker(): LexData[(meta typeId)] =
  #   (meta lexerMakerBody)
  let makerName = genSym(nskProc)
  result.add(
    newProc(
      name = makerName,
      params = @[
        nnkBracketExpr.newTree(newIdentNode("LexData"),
                               typeId)
      ],
      body = lexerMakerBody
    )
  )

  # call lexerMaker and define lexerTable as const in result

  # const (meta nameStr) = ...Maker()
  result.add(
    nnkStmtList.newTree(
      nnkConstSection.newTree(
        nnkConstDef.newTree(
          nnkPostfix.newTree(
            newIdentNode("*"),
            newIdentNode(nameStr)
          ),
          newEmptyNode(),
          nnkCall.newTree(
            makerName
          )
        )
      )
    )
  )

  when defined(nimldebug):
    echo toStrLit(result)
