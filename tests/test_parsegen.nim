import unittest
import patty
import strutils

import nimly

type
  MyTokenKind{.pure.} = enum
    PLUS
    MULTI
    NUM
    IGNORE
  MyToken = object
    case kind: MyTokenKind
    of NUM:
      val: int
    else:
      discard

proc PLUS(): MyToken =
  return MyToken(kind: MyTokenKind.PLUS)
proc MULTI(): MyToken =
  return MyToken(kind: MyTokenKind.MULTI)
proc NUM(num: int): MyToken =
  return MyToken(kind: MyTokenKind.NUM, val: num)
proc IGNORE(): MyToken =
  return MyToken(kind: MyTokenKind.IGNORE)

niml testLex[MyToken]:
  r"\+":
    return PLUS()
  r"\*":
    return MULTI()
  r"\d*":
    return NUM(parseInt(token.token))
  r"\s":
    return IGNORE()

nimy testPar[MyToken]:
  top[string]:
    plus:
      return $1
  plus[string]:
    plus PLUS plus:
      return $1 & " + " & $3
    mult:
      return $1
  mult[string]:
    mult MULTI mult:
      return "(" & $1 & " * " & $3 & ")"
    num:
      return $1
  num[string]:
    NUM:
      return $(($1).val)

test "test 1":
  var testLexer = testLex.newWithString("1 + 2 * 3")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  var
    ret: seq[MyTokenKind] = @[]
  for token in testLexer.lexIter:
    ret.add(token.kind)
  check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                 MyTokenKind.MULTI, MyTokenKind.NUM]

test "test 2":
  var testLexer = testLex.newWithString("1 + 2 * 3")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  testPar.initParser()
  check testPar.parse(testLexer) == "1 + (2 * 3)"
  testLexer.initWithString("1 + 2 * 3")
  testPar.initParser()
  check testPar.parse(testLexer) == "1 + (2 * 3)"
