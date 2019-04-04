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
  ConstActionTable* = seq[seq[int]]
  ConstGotoTable* = seq[seq[int]]
  ConstTable* = (ConstActionTable, ConstGotoTable)
  SymbolToInt*[T] = Table[Symbol[T], int]
  RuleToInt*[T] = Table[Rule[T], int]
  Parser*[T] = object
    stack: seq[State]
    table*: ParsingTable[T]
  IntToSym*[T] = Table[int, Symbol[T]]
  IntToRule*[T] = Table[int, Rule[T]]

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

proc toConst*[T](pt: ParsingTable[T],
                 sti: SymbolToInt[T],
                 rti: RuleToInt[T]): ConstTable =
  var
    cat: seq[seq[int]] = @[]
    cgt: seq[seq[int]] = @[]
  for state, ar in pt.action:
    var row: seq[int] = @[]
    for sym, ati in ar:
      let idx = sti[sym]
      var item: int
      case ati.kind
      of ActionTableItemKind.Shift:
        let s = ati.state
        item = s
      of ActionTableItemKind.Reduce:
        let r = ati.rule
        item = rti[r]
      of ActionTableItemKind.Accept:
        item = high(int)
      else:
        doAssert false
      row.inst(item, low(int), idx)
    let default: seq[int] = @[]
    cat.inst(row, default, state)

  for state, gr in pt.goto:
    var row: seq[int] = @[]
    for sym, st in gr:
      let idx = sti[sym]
      row.inst(st, low(int), idx)
    let default: seq[int] = @[]
    cgt.inst(row, default, state)

  result = (cat, cgt)

proc reconstruct*[T](cpt: ConstTable,
                     its: IntToSym[T],
                     itr: IntToRule[T]): ParsingTable[T] =
  let (cat, cgt) = cpt
  var
    rat = initTable[State, ActionRow[T]]()
    rgt = initTable[State, GotoRow[T]]()
  for rid, row in cat:
    if row.len == 0:
      continue
    var trow = initTable[Symbol[T], ActionTableItem[T]]()
    for id, ati in row:
      if ati == high(int):
        trow[its[id]] = Accept[T]()
      elif ati < 0 and ati != low(int):
        trow[its[id]] = Reduce[T](itr[ati])
      elif ati > 0:
        trow[its[id]] = Shift[T](ati)
    rat[rid] = trow
  for rid, row in cgt:
    if row.len == 0:
      continue
    var trow = initTable[Symbol[T], State]()
    for id, st in row:
      if st == low(int):
        continue
      trow[its[id]] = st
    rgt[rid] = trow
  result = ParsingTable[T](action: rat, goto: rgt)

proc `$`*[T, S](pt: ParseTree[T, S], indent: int = 0): string =
  match pt:
    Terminal(token: t):
      result = "  ".repeat(indent) & $t & "\n"
    NonTerminal(rule: r, tree: t):
      result = "  ".repeat(indent) & "rule: " & $r & "\n"
      for n in t:
        result = result & `$`(n, indent + 1)

proc add*[T](parser: var Parser[T], s: State) =
  parser.stack.add(s)

proc push*[T](parser: var Parser[T], s: State) =
  parser.add(s)

proc pop*[T](parser: var Parser[T]): State =
  return parser.stack.pop

proc top*[T](parser: Parser[T]): State =
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
      let reseted = tree[^r.len..^1]
      for i in 0..<r.len:
        discard parser.pop
        discard tree.pop
      tree.add(NonTerminal[T, S](r, reseted))
      try:
        parser.push(parser.table.goto[parser.top][r.left])
      except KeyError:
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

proc init*[T](p: var Parser[T]) =
  p.stack = @[0]
