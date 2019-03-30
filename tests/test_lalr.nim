import unittest

include nimly/lr
include nimly/lalr

let
  g = initGrammar[string](
    [
      newRule(NonTermS[string]("S"),
              NonTermS[string]("C"),NonTermS[string]("C")),
      newRule(NonTermS[string]("C"), TermS("c"), NonTermS[string]("C")),
      newRule(NonTermS[string]("C"), TermS("d")),
    ].toSet,
    NonTermS[string]("S")
  ).augument

test "test closure for lalr":
  let
    itm = LALRItem[string](rule: g.startRule, pos: 0, ahead: End[string]())
    c = closure(g, toSet[LALRItem[string]]([itm]))
    expected =  [
      itm,
      LALRItem[string](
        rule: newRule(NonTermS[string]("S"),
                      NonTermS[string]("C"),NonTermS[string]("C")),
        pos: 0,
        ahead: End[string]()
      ),
      LALRItem[string](
        rule: newRule(NonTermS[string]("C"), TermS("c"), NonTermS[string]("C")),
        pos: 0,
        ahead: TermS("c")
      ),
      LALRItem[string](
        rule: newRule(NonTermS[string]("C"), TermS("c"), NonTermS[string]("C")),
        pos: 0,
        ahead: TermS("d")
      ),
      LALRItem[string](
        rule: newRule(NonTermS[string]("C"), TermS("d")),
        pos: 0,
        ahead: TermS("c")
      ),
      LALRItem[string](
        rule: newRule(NonTermS[string]("C"), TermS("d")),
        pos: 0,
        ahead: TermS("d")
      )
    ].toSet

  check c == expected
