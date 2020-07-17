import unittest

import nimly
import lexer_global_var

test "test global var":
  var lexer = lexGlobalVar.newWithString(
    "0\n  1\n  1\n    2\n\n  1\n   2"
  )
  let expected = @[
    Num("0"),
    Indent(1),
    Num("1"),
    Indent(1),
    Num("1"),
    Indent(2),
    Num("2"),
    Indent(1),
    Num("1"),
    Indent(2),
    Num("2")
  ]
  var ret: seq[Token] = @[]
  for s in lexer.lexIter:
    ret.add(s)
  check ret == expected
