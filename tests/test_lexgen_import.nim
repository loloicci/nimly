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


niml testLex0[string]:
  r"if":
    return "acc"

test "test macro niml (if)":
  check testLex0.doesAccept("if")
  check testLex0.accProc("if")(LToken(token: "if")) == "acc"
  check (not testLex0.doesAccept("i"))
  check (not testLex0.doesAccept("else"))
  check (not testLex0.doesAccept("iff"))

niml testLex1[string]:
  r"if":
    return token.token
  r"else":
    return "acc"

test "test macro niml (if/elif)":
  check testLex1.doesAccept("if")
  check testLex1.accProc("if")(LToken(token: "if")) == "if"
  check (not testLex1.doesAccept("i"))
  check testLex1.doesAccept("else")
  check testLex1.accProc("else")(LToken(token: "else")) == "acc"
  check (not testLex1.doesAccept("iff"))
