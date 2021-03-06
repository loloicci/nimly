import unittest
import patty

import nimly

variant Token:
  CHARS(val: string)
  IGNORE

niml testLex[Token]:
  r"\w+":
    return CHARS(token.token)
  r"\s":
    return IGNORE()

nimy testPar[Token]:
  top[seq[string]]:
    word{}:
      return $1
  word[string]:
    CHARS:
      return ($1).val

test "parser works":
  var testLexer = testLex.newWithString("This is a test")
  testLexer.ignoreIf = proc(r: Token): bool = r.kind == TokenKind.IGNORE
  var parser = testPar.newParser()
  check parser.parse(testLexer) == @["This", "is", "a", "test"]

test "empty string does not cause error":
  var testLexer = testLex.newWithString("")
  testLexer.ignoreIf = proc(r: Token): bool = r.kind == TokenKind.IGNORE
  var parser = testPar.newParser()
  check parser.parse(testLexer).len == 0
