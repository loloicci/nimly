import unittest
import patty
import strutils

import nimly

variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  DOT
  IGNORE

niml testLex[MyToken]:
  r"\+":
    return PLUS()
  r"\*":
    return MULTI()
  r"\d":
    return NUM(parseInt(token.token))
  r"\.":
    return DOT()
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
    NUM DOT[] NUM{}:
      result = ""
      result.add(($1).val)
      if ($2).len > 0:
        result.add(".")
      for tkn in $3:
        result.add(tkn.val)

test "test 1":
  var testLexer = testLex.newWithString("1 + 42 * 101010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  var
    ret: seq[MyTokenKind] = @[]
  for token in testLexer.lexIter:
    ret.add(token.kind)
  check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                 MyTokenKind.NUM, MyTokenKind.MULTI,
                 MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM,
                 MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM]

test "test 2":
  var testLexer = testLex.newWithString("1 + 42 * 101010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  testPar.init()
  check testPar.parse(testLexer) == "1 + (42 * 101010)"
  testLexer.initWithString("1 + 42 * 1010")
  testPar.init()
  check testPar.parse(testLexer) == "1 + (42 * 1010)"

test "test 2":
  var testLexer = testLex.newWithString("1 + 42 * 1.01010")
  testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  testPar.init()
  check testPar.parse(testLexer) == "1 + (42 * 1.01010)"
  testLexer.initWithString("1. + 4.2 * 101010")
  testPar.init()
  check testPar.parse(testLexer) == "1. + (4.2 * 101010)"

