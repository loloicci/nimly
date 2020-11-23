import unittest

import nimly

import state_parser_with_empty_lr

test "test state":
  var lexer = testStateLex.newWithString("if test + 1 then { true } else { 2 * ( test + 3 ) }")
  lexer.ignoreIf = proc(r: StateToken): bool = r.kind == StateTokenKind.SIGNORE

  var parser = testStatePar.newParser()
  check parser.parse(lexer) == "IF(test+1)THEN{true}ELSE{(2*(test+3))}"

test "test state with empty":
  var lexer = testStateLex.newWithString("if test + 1 then { true }")
  lexer.ignoreIf = proc(r: StateToken): bool = r.kind == StateTokenKind.SIGNORE

  var parser = testStatePar.newParser()
  check parser.parse(lexer) == "IF(test+1)THEN{true}"

test "test state with empty2":
  var lexer = testStateLex.newWithString("if then { true }")
  lexer.ignoreIf = proc(r: StateToken): bool = r.kind == StateTokenKind.SIGNORE

  var parser = testStatePar.newParser()
  check parser.parse(lexer) == "IF()THEN{true}"
