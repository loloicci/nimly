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
  check sa != deadState
  check sab != deadState
  check acc != deadState
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

proc doesAccept[T](dfa: DFA[T], str: string): bool =
  var state = dfa.start
  for c in str:
    if (not dfa.tran[state].haskey(c)):
      return false
    state = dfa.tran[state][c]
  return dfa.accepts.hasKey(state)

proc finalState[T](ld: LexData[T], str: string): int =
  result = 0
  for c in str:
    result = ld.nextState(result, c)
    if result == deadState:
      return

proc doesAccept[T](ld: LexData[T], str: string): bool =
  let fs = finalState[T](ld, str)
  if fs == deadState:
    return false
  return ld.dba[fs].accept.kind == AcceptKind.Acc

proc accProc[T](ld: LexData[T], str: string): AccProc[T] =
  let fs = finalState[T](ld, str)
  assert ld.dba[fs].accept.kind == AcceptKind.Acc
  return ld.dba[fs].accept.fun

proc makeDFAForTest(re: string): DFA[string] =
  var nextPos = 0
  let
    rt = re.convertToReSynTree(nextPos)
    accs = rt.accPos

  var app = newAccPosProc[string]()

  for acc in accs:
    app[acc] = strid

  let
    lexRe = LexRe[string](
      st: rt,
      accPosProc: app)

  return lexRe.makeDFA

test "test convertToSynTree (test)":
  let dfa = r"test".makeDFAForTest
  check dfa.doesAccept("test")
  check (not dfa.doesAccept("testing"))
  check (not dfa.doesAccept("tes"))
  check (not dfa.doesAccept(""))

test "test convertToSynTree (([a..c][^a..c])+)":
  let dfa = r"([a..c][^a..c])+".makeDFAForTest
  check dfa.doesAccept("adbecf")
  check dfa.doesAccept("c ")
  check (not dfa.doesAccept(""))
  check (not dfa.doesAccept("cast"))

test "test convertToSynTree ((abc)?)":
  let dfa = r"(abc)?".makeDFAForTest
  check dfa.doesAccept("abc")
  check dfa.doesAccept("")
  check (not dfa.doesAccept("abcabc"))
  check (not dfa.doesAccept("abcd"))
  check (not dfa.doesAccept("ab"))

test r"test convertToSynTree ([^\s]?)":
  let dfa = r"[^\s]?".makeDFAForTest
  check dfa.doesAccept("a")
  check dfa.doesAccept("")
  check (not dfa.doesAccept(" "))

test r"test convertToSynTree (\w*)":
  let dfa = r"\w*\\".makeDFAForTest
  check dfa.doesAccept("test-01\\")
  check (not dfa.doesAccept("test_02\\"))

test "test convertToSynTree ((a{2,4}b|c{2,4}d)+e)":
  let dfa = r"(a{2,4}b|c{2,4}d)+e".makeDFAForTest
  check dfa.doesAccept("aabe")
  check dfa.doesAccept("ccde")
  check dfa.doesAccept("aaabe")
  check dfa.doesAccept("cccde")
  check dfa.doesAccept("aaaabe")
  check dfa.doesAccept("ccccde")
  check dfa.doesAccept("aabccccdaaaabccde")
  check dfa.doesAccept("aaabcccdccccdccde")
  check (not dfa.doesAccept("e"))
  check (not dfa.doesAccept("abe"))
  check (not dfa.doesAccept("cbe"))
  check (not dfa.doesAccept("aaaaabe"))
  check (not dfa.doesAccept("cccccde"))

test "test convertToSynTree (.*)":
  let dfa = r".*".makeDFAForTest
  check dfa.doesAccept("test")
  check dfa.doesAccept("this is test.")
  check dfa.doesAccept("tess \n is \n test.")
  check dfa.doesAccept("")
  check dfa.doesAccept("()*")

test "test macro niml (if)":
  niml testLex[string]:
    r"if":
      return "acc"
  check testLex.doesAccept("if")
  check testLex.accProc("if")(LToken(token: "if")) == "acc"
  check (not testLex.doesAccept("i"))
  check (not testLex.doesAccept("else"))
  check (not testLex.doesAccept("iff"))

test "test macro niml (if)":
  niml testLex[string]:
    ## comment
    r"if":
      return token.token
    r"else":
      return "acc"
  check testLex.doesAccept("if")
  check testLex.accProc("if")(LToken(token: "if")) == "if"
  check (not testLex.doesAccept("i"))
  check testLex.doesAccept("else")
  check testLex.accProc("else")(LToken(token: "else")) == "acc"
  check (not testLex.doesAccept("iff"))
