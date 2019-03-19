import unittest
import streams

import nimly/lextypes
import nimly/lexgen
import nimly/lexer

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

test "test macro nimly (if/else)":
  niml testLex[string]:
    r"if":
      return token.token
    r"else":
      return "acc"
    r"\s":
      return ""
  var testLexer = testLex.newWithString("""if
else if
else""")
  var ret: seq[string] = @[]
  for s in testLexer.lexIter(proc(r: string): bool = false):
    ret.add(s)
  check ret == @["if", "", "acc", "", "if", "", "acc"]

  testLexer = testLex.newWithString("""if
else if
else""")
  ret = @[]
  for s in testLexer.lexIter(proc(r: string): bool = r == ""):
    ret.add(s)
  check ret == @["if", "acc", "if", "acc"]
