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
    ].toHashSet,
    NonTermS[string]("S")
  ).augument

  g415 = initGrammar[string](
    [
      newRule(NonTermS[string]("S"), NonTermS[string]("R")),
      newRule(NonTermS[string]("S"),
              NonTermS[string]("L"), TermS("="), NonTermS[string]("R")),
      newRule(NonTermS[string]("L"), TermS("*"), NonTermS[string]("R")),
      newRule(NonTermS[string]("L"), TermS("id")),
      newRule(NonTermS[string]("R"), NonTermS[string]("L")),
    ].toHashSet,
    NonTermS[string]("S")
  ).augument

test "test closure for lalr":
  let
    itm = LALRItem[string](rule: g.startRule, pos: 0, ahead: End[string]())
    c = closure(g, toHashSet[LALRItem[string]]([itm]))
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
    ].toHashSet

  check c == expected

proc contains[T](itms: LALRItems[T], itm: LRItem[T]): bool =
  result = false
  for i in itms:
    if i.toLRItem == itm:
      return true

test "test make LALR kernel":
  let
    (cc, tt) = g415.makeCanonicalCollection
    kernel = cc.filterKernel
  check kernel.card == 10
  for i, itms in kernel:
    if itms.contains(
      LRItem[string](
        rule: newRule(NonTermS[string]("S"),
                      NonTermS[string]("L"), TermS("="),
                      NonTermS[string]("R")),
        pos: 1
      )
    ):
      check itms.card == 2
    else:
      check itms.card == 1
  let lalrKernel = kernel.toLALRKernel(g415, tt)
  for i, itms in lalrKernel:
    if itms.contains(
      LRItem[string](
        rule: newRule(NonTermS[string]("R"), NonTermS[string]("L")),
        pos: 1
      )
    ) or itms.contains(
      LRItem[string](
        rule: newRule(NonTermS[string]("L"),
                      TermS("*"), NonTermS[string]("R")),
        pos: 1
      )
    ) or itms.contains(
      LRItem[string](
        rule: newRule(NonTermS[string]("L"), TermS("id")),
        pos: 1
      )
    ) or itms.contains(
      LRItem[string](
        rule: newRule(NonTermS[string]("L"),
                      TermS("*"), NonTermS[string]("R")),
        pos: 2
      )
    ):
      check itms.card == 2
    else:
      check itms.card == 1


