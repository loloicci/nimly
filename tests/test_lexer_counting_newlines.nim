import strutils
import unittest

import nimly


type
  TokenType = enum
    OP
    INT
    IGNORE

  Token = object
    typ: TokenType
    val: string
    line: int
    col: int


niml testLex[Token]:
  r"\+|-|\*|/":
    return Token(typ: OP, val: token.token,
                 line: token.lineNum, col: token.colNum)
  r"\d+":
    return Token(typ: INT, val: token.token,
                 line: token.lineNum, col: token.colNum)
  r"\s":
    return Token(typ: IGNORE, val: "",
                 line: token.lineNum, col: token.colNum)


test "test lexer counting newline (Issue #34)":
  let
     str = "22 / \n 11 +\n40"
     expected = @[
       Token(typ: INT, val: "22", line: 1, col: 0),
       Token(typ: OP, val: "/", line: 1, col: 3),
       Token(typ: INT, val: "11", line: 2, col: 1),
       Token(typ: OP, val: "+", line: 2, col: 4),
       Token(typ: INT, val: "40", line: 3, col: 0)
     ]

  var
    calcLexer = testLex.newWithString(str)
    ret: seq[Token] = @[]

  calcLexer.ignoreIf = proc(r: Token): bool = r.val == ""

  for token in calcLexer.lexIter:
    ret.add(token)

  check ret == expected
