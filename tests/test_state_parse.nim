import unittest

import nimly

import state_parser

test "test state":
  var lexer = testStateLex.newWithString("if test + 1 then { true } else { 2 * ( test + 3 ) }")
  lexer.ignoreIf = proc(r: StateToken): bool = r.kind == StateTokenKind.SIGNORE

  var parser = testStatePar.newParser()
  check parser.parse(lexer) == "IF(test+1)THEN{true}ELSE{(2*(test+3))}"

test "test state (read from file)":
  var lexer = testStateLex.open("tests/state_example.txt")
  lexer.ignoreIf = proc(r: StateToken): bool = r.kind == StateTokenKind.SIGNORE

  var parser = testStatePar.newParser()
  check parser.parse(lexer) == "IF(test+1)THEN{true}ELSE{(2*(test+3))}"
