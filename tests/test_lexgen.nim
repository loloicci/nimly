import unittest

include nimly/lexgen

proc strid(token: LToken): string =
  return token.token

let
  dbookExampleSynT = Bin(bcat,
                         ~Bin(bcat,
                              ~Bin(bcat,
                                   ~Bin(bcat,
                                        ~Star(~Bin(bor,
                                                   ~Term(Char(1, Real('a'))),
                                                   ~Term(Char(2, Real('b'))))
                                        ),
                                        ~Term(Char(3, Real('a')))),
                                   ~Term(Char(4, Real('b')))),
                              ~Term(Char(5, Real('b')))),
                         ~Term(Char(6, End())))

  dbookExampleLexRe = LexRe[string](
    st: dbookExampleSynT,
    accPosProc: {6: strid}.newTable
  )

  dbookExampleDFA3p36 = DFA[string](
    start: 0,
    accepts: {
      4: strid
      }.newTable,
    stateNum: 5,
    tran: {
      0: {'a': 1, 'b': 2}.newTable(),
      1: {'a': 1, 'b': 3}.newTable(),
      2: {'a': 1, 'b': 2}.newTable(),
      3: {'a': 1, 'b': 4}.newTable(),
      4: {'a': 1, 'b': 2}.newTable(),
      }.newTable
    )

test "test makeFollowposTable (Dragonbook 3.9.4)":
  var
    one = initSet[Pos]()
    two = initSet[Pos]()
    three = initSet[Pos]()
    four = initSet[Pos]()
    five = initSet[Pos]()
  one.incl(1)
  one.incl(2)
  one.incl(3)
  two = one
  three.incl(4)
  four.incl(5)
  five.incl(6)
  let
    followpos: array[0..4, (Pos, HashSet[Pos])] = {
      1: one,
      2: two,
      3: three,
      4: four,
      5: five
    }

  check dbookExampleSynT.makeFollowposTable == followpos.newTable

test "test correctChar":
  check dbookExampleSynT.collectChar == {'a', 'b'}

template checkDFA[A](dfa: DFA[A]) =
  let
    sa = dfa.tran[dfa.start]['a']
    sab = dfa.tran[sa]['b']
    acc = dfa.tran[sab]['b']
  check dfa.stateNum == 4
  check dfa.tran[dfa.start]['b'] == dfa.start
  check dfa.tran[sa]['a'] == sa
  check dfa.tran[sab]['a'] == sa
  check dfa.accepts.haskey(acc)
  check dfa.tran[acc]['a'] == sa
  check dfa.tran[acc]['b'] == dfa.start

test "test makeDFA (Dragonbook 3.9.5)":
  let dfa = makeDFA[string](dbookExampleLexRe)
  checkDFA(dfa)

test "test minimizeStates (id)":
  let dfa = makeDFA[string](dbookExampleLexRe)
  var
    acc = initSet[DState]()
    other = initSet[DState]()
  for i in 0..<dfa.stateNum:
    if dfa.accepts.haskey(i):
      acc.incl(i)
    else:
      other.incl(i)

  let minDFA = dfa.minimizeStates(@[acc, other])

  checkDFA(minDFA)

test "test minimizeStates":
  let dfa = dbookExampleDFA3p36
  var
    acc = initSet[DState]()
    other = initSet[DState]()
  for i in 0..<dfa.stateNum:
    if dfa.accepts.haskey(i):
      acc.incl(i)
    else:
      other.incl(i)

  let minDFA = dfa.minimizeStates(@[acc, other])

  checkDFA(minDFA)

test "test convertToLexData":
  let
    dfa = makeDFA[string](dbookExampleLexRe)
    ld = convertToLexData[string](dfa)
    sa = ld.nextState(0, 'a')
    sab = ld.nextState(sa, 'b')
    acc = ld.nextState(sab, 'b')
  check ld.nc.len == 4
  check ld.nextState(0, 'b') == 0
  check ld.nextState(sa, 'a') == sa
  check ld.nextState(sab, 'a') == sa
  check ld.nextState(acc, 'a') == sa
  check ld.nextState(acc, 'b') == 0
  for i, dba in ld.dba:
    if i == acc:
      check dba.accept.kind == AcceptKind.Acc
    else:
      check dba.accept.kind == AcceptKind.NotAcc

