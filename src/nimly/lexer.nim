import lexbase
import streams

import lextypes
import lexgen

type
  NimlLexer*[T] = object of BaseLexer
    data*: LexData[T]
    ignoreIf*: proc(r: T): bool
  NimlError* = object of Exception
  NimlEOFError* = object of NimlError

proc open*[T](data: LexData[T], path: string): NimlLexer[T] =
  result = NimlLexer[T](data: data)
  result.open(openFileStream(path))

proc newWithString*[T](data: LexData[T], str: string): NimlLexer[T] =
  result = NimlLexer[T](data: data)
  result.open(newStringStream(str))

proc open*[T](lexer: var NimlLexer[T], path: string) =
  lexer.open(openFileStream(path))

proc initWithString*[T](lexer: var NimlLexer[T], str: string) =
  lexer.open(newStringStream(str))

export lexbase.close

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
    of '\c':
      pos = nl.handleCR(pos)
    else:
      inc(pos)
    state = nl.data.nextState(state, c)
    when defined(nimldebug):
      echo "read:" & c
      echo "state:" & $state
    if nl.data.isAcc(state):
      lastAccToken = token
      lastAccState = state
      lastAccPos = pos
    assert c != EndOfFile or state == -1, "invalid EOF while lexing"

  doassert lastAccState != deadState, "LexError:\n" & lineInfo

  ltoken.token = lastAccToken

  result = nl.data.dba[lastAccState].accept.fun(ltoken)

  nl.bufpos = lastAccPos
  when defined(nimldebug):
    echo "--lex end--"
    echo "token:" & lastAccToken
    try:
      echo "result:" & $result
    except:
      discard

proc lexNext*[T](nl: var NimlLexer[T]): T =
  while nl.buf[nl.bufpos] != EndOfFile:
    result = nl.lex
    if not nl.ignoreIf(result):
      return
  raise newException(NimlEOFError, "read EOF")

iterator lexIter*[T](nl: var NimlLexer[T]): T =
  while nl.buf[nl.bufpos] != EndOfFile:
    yield nl.lexNext
