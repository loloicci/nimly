import unittest
import patty
import strutils
import macros

import nimly

## variant is defined in patty
variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  DOT
  LPAREN
  RPAREN
  IGNORE

niml testLex[MyToken]:
  r"\(":
    return LPAREN()
  r"\)":
    return RPAREN()
  r"\+":
    return PLUS()
  r"\*":
    return MULTI()
  r"\d+":
    return NUM(parseInt(token.token))
  r"\s":
    return IGNORE()

nimy testPar[MyToken]:
  top[NimNode]:
    plus:
      return $1

  plus[NimNode]:
    mult PLUS plus:
      return newCall(
        ident("+"),
        $1,
        $3
      )

    mult:
      return $1

  mult[NimNode]:
    num MULTI mult:
      return newCall(
        ident("*"),
        $1,
        $3
      )

    num:
      return $1

  num[NimNode]:
    LPAREN plus RPAREN:
      return $2

    NUM:
      return newIntLitNode(($1).val)

macro calculate(str: untyped): untyped =
  var
    lexer = testLex.newWithString($str)
  lexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
  var
     parser = testPar.newParser()
  return parser.parse(lexer)

test "test compile-time parser":
  check calculate("(20 + 1) * 2") == 42
