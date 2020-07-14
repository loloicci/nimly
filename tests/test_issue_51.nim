import unittest

import patty
import nimly

variant MyToken:
  tSYM(str: string)

niml lexer1[MyToken]:
  "[^ \t\r\n]+": tSYM(token.token)

niml lexer2[MyToken]:
  r"\S+":
    return tSYM(token.token)

test "test [^...] in regex":
  var testLexer = lexer1.newWithString("loloi<<1")
  testLexer.ignoreIf = proc(r: MyToken): bool = false
  var ret: seq[MyToken] = @[]
  for s in testLexer.lexIter:
    ret.add(s)
  check ret == @[tSYM("loloi<<1")]
  testLexer.close

test "test [^...] in regex (exception)":
  var testLexer = lexer1.newWithString("loloi << 1")
  testLexer.ignoreIf = proc(r: MyToken): bool = false
  check testLexer.lexNext == tSYM("loloi")
  expect LexError:
    discard testLexer.lexNext

test r"test \S in regex (exception)":
  var testLexer = lexer2.newWithString("loloi<<1")
  testLexer.ignoreIf = proc(r: MyToken): bool = false
  var ret: seq[MyToken] = @[]
  for s in testLexer.lexIter:
    ret.add(s)
  check ret == @[tSYM("loloi<<1")]
  testLexer.close

test r"test \S in regex (exception)":
  var testLexer = lexer2.newWithString("loloi << 1")
  testLexer.ignoreIf = proc(r: MyToken): bool = false
  check testLexer.lexNext == tSYM("loloi")
  expect LexError:
    discard testLexer.lexNext

