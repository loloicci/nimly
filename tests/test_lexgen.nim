import unittest

include nimly/lexgen

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

  dbookExampleDFA3p36 = DFA(
    start: DState(0),
    accepts: {DState(4)},
    states: {DState(0), DState(1), DState(2), DState(3), DSTate(4)},
    tran: {
      DState(0): {'a': DState(1), 'b': DState(2)}.newTable(),
      DState(1): {'a': DState(1), 'b': DState(3)}.newTable(),
      DState(2): {'a': DState(1), 'b': DState(2)}.newTable(),
      DState(3): {'a': DState(1), 'b': DState(4)}.newTable(),
      DState(4): {'a': DState(1), 'b': DState(2)}.newTable(),
      }.newTable()
    )

test "test makeFollowposTable (Dragonbook 3.9.4)":
  let
    followpos: array[0..4, (Pos, set[Pos])] = {
      Pos(1): {Pos(1), Pos(2), Pos(3)},
      Pos(2): {Pos(1), Pos(2), Pos(3)},
      Pos(3): {Pos(4)},
      Pos(4): {Pos(5)},
      Pos(5): {Pos(6)}
                }

  check dbookExampleSynT.makeFollowposTable == followpos.newTable

test "test correctChar":
  check dbookExampleSynT.collectChar == {'a', 'b'}

template checkDFA(dfa: DFA) =
  let sa = dfa.tran[dfa.start]['a']
  let sab = dfa.tran[sa]['b']
  let acc = dfa.tran[sab]['b']
  check dfa.states.card == 4
  check dfa.tran[dfa.start]['b'] == dfa.start
  check dfa.tran[sa]['a'] == sa
  check dfa.tran[sab]['a'] == sa
  check dfa.accepts == {acc}
  check dfa.tran[acc]['a'] == sa
  check dfa.tran[acc]['b'] == dfa.start


test "test makeDFA (Dragonbook 3.9.5)":
  let dfa = dbookExampleSynT.makeDFA
  checkDFA(dfa)

test "test minimizeStates (id)":
  let dfa = dbookExampleSynT.makeDFA.minimizeStates
  checkDFA(dfa)

test "test minimizeStates":
  let dfa = dbookExampleDFA3p36.minimizeStates
  checkDFA(dfa)
