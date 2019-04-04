import unittest

import nimly
import parser_415
import parser_415_lr

## to check LR:CONFLICT in -d:nimydebug mode

test "test lalr":
  var lexer415 = lex415.newWithString("left")
  lexer415.ignoreIf = proc(r: MyTerm): bool = r.kind == MyTermKind.IGNORE
  psr415LALR.init
  check parser_415.parse(psr415LALR, lexer415) == "left"

test "test lr":
  var lexer415 = lex415.newWithString("left")
  lexer415.ignoreIf = proc(r: MyTerm): bool = r.kind == MyTermKind.IGNORE
  psr415LR.init
  check parser_415_lr.parse(psr415LR, lexer415) == "left"
