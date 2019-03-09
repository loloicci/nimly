import unittest

import nimly/lextypes
import nimly/lexgen

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


test "test macro nimly (if)":
  nimly testLex[string]:
    r"if":
      return "acc"
  check testLex.doesAccept("if")
  check testLex.accProc("if")(LToken(token: "if")) == "acc"
  check (not testLex.doesAccept("i"))
  check (not testLex.doesAccept("else"))
  check (not testLex.doesAccept("iff"))

test "test macro nimly (if)":
  nimly testLex[string]:
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
