import tables
import strutils

import patty

import lextypes
import lexer
import parsetypes

type
  State* = int
  ActionTableItemKind* {.pure.} = enum
    Shift
    Reduce
    Accept
    Error
  ActionTableItem*[T] = object
    case kind*: ActionTableItemKind:
    of ActionTableItemKind.Shift:
      state*: State
    of ActionTableItemKind.Reduce:
      rule*: Rule[T]
    else:
      discard

proc `$`*[T](i: ActionTableItem[T]): string =
  match i:
    Shift(state: s):
      return "Shift(" & $s & ")"
    Reduce(rule: r):
      return "Reduce(" & $r & ")"
    Accept:
      return "Accept"
    Error:
      return "Error"

proc Shift*[T](state: State): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Shift, state: state)

proc Reduce*[T](rule: Rule[T]): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Reduce, rule: rule)

proc Accept*[T](): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Accept)

proc Error*[T](): ActionTableItem[T] =
  return ActionTableItem[T](kind: ActionTableItemKind.Error)

type
  ActionRow*[T] = Table[Symbol[T], ActionTableItem[T]]
  ActionTable*[T] = Table[State, ActionRow[T]]
  GotoRow*[T] = Table[Symbol[T], State]
  GotoTable*[T] = Table[State, GotoRow[T]]
  ParsingTable*[T] = object
    action*: ActionTable[T]
    goto*: GotoTable[T]
  ConstActionTable = seq[seq[int]]
  ConstGotoTable = seq[seq[int]]
  ConstTable* = (ConstActionTable, ConstGotoTable)
  SymbolToInt[T] = Table[Symbol[T], int]
  RuleToInt[T] = Table[Rule[T], int]
  Parser*[T] = object
    stack: seq[State]
    table: ParsingTable[T]
  IntToSym[T] = Table[int, Symbol[T]]
  IntToRule[T] = Table[int, Rule[T]]

proc `$`*[T](at: ActionTable[T]): string =
  result = "\nActionTable:\n--------\n"
  for s, row in at:
    result = result & $s & ":" & $row & "\n"
  result = result & "--------\n"

proc `$`*[T](gt: GotoTable[T]): string =
  result = "\nGotoTable:\n--------\n"
  for s, row in gt:
    result = result & $s & ":" & $row & "\n"
  result = result & "--------\n"

variantp ParseTree[T, S]:
  Terminal(token: T)
  NonTerminal(rule: Rule[S], tree: seq[ParseTree[T, S]])

proc inst[T](sq: var seq[T], item: T, default: T, index: int) =
  for i in 0..(index - sq.len):
    sq.add(default)
  sq[index] = item

proc `$`*[T, S](pt: ParseTree[T, S], indent: int = 0): string =
  match pt:
    Terminal(token: t):
      result = "  ".repeat(indent) & $t & "\n"
    NonTerminal(rule: r, tree: t):
      result = "  ".repeat(indent) & "rule: " & $r & "\n"
      for n in t:
        result = result & `$`(n, indent + 1)

proc add[T](parser: var Parser[T], s: State) =
  parser.stack.add(s)

proc push[T](parser: var Parser[T], s: State) =
  parser.add(s)

proc pop[T](parser: var Parser[T]): State =
  return parser.stack.pop

proc top[T](parser: Parser[T]): State =
  return parser.stack[parser.stack.high]

proc parseImpl*[T, S](parser: var Parser[S],
                      lexer: var NimlLexer[T]): ParseTree[T, S] =
  var tree: seq[ParseTree[T, S]] = @[]
  var token = lexer.lexNext
  var symbol = TermS[S](token.kind)
  while true:
    when defined(nimydebug):
      echo "parser stack:" & $parser.stack
      echo "read token:" & $symbol
    var action: ActionTableItem[S]
    try:
      action = parser.table.action[parser.top][symbol]
    except KeyError:
      var msg: string = "Unexpected token" & $symbol & "is passed."
      try:
        msg = msg & "\ntoken: " & $token
      except:
        discard
      raise newException(NimyActionError, msg)
    except:
      raise
    when defined(nimydebug):
      echo action
    case action.kind
    of ActionTableItemKind.Shift:
      let s = action.state
      tree.add(Terminal[T, S](token))
      try:
        token = lexer.lexNext
        symbol = TermS[S](token.kind)
      except NimlEOFError:
        symbol = End[S]()
      except:
        raise
      parser.push(s)
    of ActionTableItemKind.Reduce:
      let r = action.rule
      let reseted = tree[^r.lenWithoutEmpty..^1]
      for i in 0..<r.lenWithoutEmpty:
        discard parser.pop
        discard tree.pop
      tree.add(NonTerminal[T, S](r, reseted))
      try:
        parser.push(parser.table.goto[parser.top][r.left])
      except KeyError:
        when defined(nimydebug):
          echo "Parser:"
          echo parser
          echo "left:"
          echo r.left
        let msg = "Nimy Internal Error (goto key error)"
        raise newException(NimyGotoError, msg)
      except:
        raise
    of ActionTableItemKind.Accept:
      when defined(nimydebug):
        if tree.len == 1:
          echo tree[0]
        else:
          echo tree
      doAssert tree.len == 1, "Error, parsing result is wrong."
      return NonTerminal[T, S](rule = Rule[S](), tree = tree)
    of ActionTableItemKind.Error:
      doAssert false, "Error, Must be implemented."
  assert false, "Something wrong with parser."

proc newParser*[T](t: ParsingTable[T]): Parser[T] =
  result = Parser[T](stack: @[0], table: t)
  result.init()

proc init*[T](p: var Parser[T]) =
  p.stack = @[0]
