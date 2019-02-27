import unittest

include nimly/lexgen

let dbookExample = Bin(bcat,
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


test "test makeFollowposTable (Dragonbook 3.9.4)":
  let
    followpos: array[0..4, (Pos, set[Pos])] = {
      Pos(1): {Pos(1), Pos(2), Pos(3)},
      Pos(2): {Pos(1), Pos(2), Pos(3)},
      Pos(3): {Pos(4)},
      Pos(4): {Pos(5)},
      Pos(5): {Pos(6)}
                }

  check dbookExample.makeFollowposTable == followpos.newTable

test "test correctChar":
  check dbookExample.collectChar == {'a', 'b'}

test "test makeDFA (Dragonbook 3.9.5)":
  let dfa = dbookExample.makeDFA
  check dfa.states.card == 4
  check dfa.tran[dfa.start]['b'] == dfa.start
  let sa = dfa.tran[dfa.start]['a']
  check dfa.tran[sa]['a'] == sa
  let sab = dfa.tran[sa]['b']
  check dfa.tran[sab]['a'] == sa
  let acc = dfa.tran[sab]['b']
  check dfa.accepts == {acc}
  check dfa.tran[acc]['a'] == sa
  check dfa.tran[acc]['b'] == dfa.start
