import lexbase
import streams

import lextypes
import lexgen

type
  NimlLexer*[T] = object of BaseLexer
    data*: LexData[T]
    ignoreIf*: proc(r: T): bool
    setUp*: proc() {.nimcall.}
    tearDown*: proc() {.nimcall.}
  NimlError* = object of Exception
  NimlEOFError* = object of NimlError

proc newNimlLexer[T](data: LexData[T]): NimlLexer[T] =
  result = NimlLexer[T](
    data: data,
    ignoreIf: proc(r: T): bool = false,
    setUp: data.setUp,
    tearDown: data.tearDown,
  )

proc open*[T](data: LexData[T], path: string): NimlLexer[T] =
  result = newNimlLexer(data)
  result.open(openFileStream(path))
  result.setUp()

proc newWithString*[T](data: LexData[T], str: string): NimlLexer[T] =
  result = newNimlLexer(data)
  result.open(newStringStream(str))
  result.setUp()

proc open*[T](lexer: var NimlLexer[T], path: string) =
  lexer.open(openFileStream(path))
  lexer.setUp()

proc initWithString*[T](lexer: var NimlLexer[T], str: string) =
  lexer.open(newStringStream(str))
  lexer.setUp()

proc close*[T](lexer: var NimlLexer[T]) =
  lexer.data.tearDown()
  lexbase.close(lexer)

proc lex*[T](nl: var NimlLexer[T]): T =
  let
    colNum = nl.getColNumber(nl.bufpos)
    lineNum = nl.lineNumber
    lineInfo = nl.getCurrentLine
  var
    token: string = ""
    lastAccToken: string = ""
    state: State = 0
    lastAccState: State = deadState
    pos = nl.bufpos
    lastAccPos: int = -1
    lastAccLine: int = -1
    ltoken = LToken(colNum: colNum, lineNum: lineNum, lineInfo: lineInfo)
  when defined(nimldebug):
    echo "--lex start--"
    echo state
  while state != deadState:
    let c = nl.buf[pos]
    token &= c
    case c
    of '\L':
      pos = nl.handleLF(pos)
      when defined(nimldebug):
        echo "handleLF"
    of '\c':
      pos = nl.handleCR(pos)
      when defined(nimldebug):
        echo "handleCR"
    else:
      inc(pos)
      when defined(nimldebug):
        echo "handleOther"

    state = nl.data.nextState(state, c)
    when defined(nimldebug):
      echo "read:" & c
      echo "state:" & $state
    if nl.data.isAcc(state):
      lastAccToken = token
      lastAccState = state
      lastAccPos = pos
      lastAccLine = nl.lineNumber
    if c == EndOfFile and lastAccState == -1:
      raise newException(LexError, "invalid EOF while lexing")

  if lastAccState == -1:
    raise newException(LexError, "LexError:\n" & lineInfo)

  ltoken.token = lastAccToken

  result = nl.data.dba[lastAccState].accept.fun(ltoken)

  nl.bufpos = lastAccPos
  nl.lineNumber = lastAccLine
  when defined(nimldebug):
    echo "--lex end--"
    echo "token:" & lastAccToken
    try:
      echo "result:" & $result
    except:
      discard

proc isEmpty*[T](nl: NimlLexer[T]): bool =
  nl.buf[nl.bufpos] == EndOfFile

proc lexNext*[T](nl: var NimlLexer[T]): T =
  while nl.buf[nl.bufpos] != EndOfFile:
    result = nl.lex
    if not nl.ignoreIf(result):
      return
  raise newException(NimlEOFError, "read EOF")

iterator lexIter*[T](nl: var NimlLexer[T]): T =
  while nl.buf[nl.bufpos] != EndOfFile:
    yield nl.lexNext
