import unittest

include nimly/lexgen

test "makeFollowposTable (Dragonbook 3.9.4)":
  let
    example = Bin(bcat,
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

    followpos: array[0..4, (Pos, set[Pos])] = {
      Pos(1): {Pos(1), Pos(2), Pos(3)},
      Pos(2): {Pos(1), Pos(2), Pos(3)},
      Pos(3): {Pos(4)},
      Pos(4): {Pos(5)},
      Pos(5): {Pos(6)}
                }

  check example.makeFollowposTable == followpos.newTable
