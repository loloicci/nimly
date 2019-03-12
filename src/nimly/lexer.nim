import lexbase
import streams

import lextypes
import lexgen

type
  NimlLexer[T] = object of BaseLexer
    data: LexData[T]

proc open*[T](data: LexData[T], path: string): NimlLexer[T] =
  result = NimlLexer[T](data: data)
  result.open(openFileStream(path))

proc newWithString*[T](data: LexData[T], str: string): NimlLexer[T] =
  result = NimlLexer[T](data: data)
  result.open(newStringStream(str))

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
    if nl.data.isAcc(state):
      lastAccToken = token
      lastAccState = state
      lastAccPos = pos
    assert c != EndOfFile or state == -1, "invalid EOF while lexing"

  doassert lastAccState != deadState, "LexError:\n" & lineInfo

  ltoken.token = lastAccToken

  result = nl.data.dba[lastAccState].accept.fun(ltoken)

  nl.bufpos = lastAccPos

iterator lexIter*[T](nl: var NimlLexer[T], ignoreIf: proc(r: T): bool): T =
  while nl.buf[nl.bufpos] != EndOfFile:
    let ret = nl.lex
    if not ignoreIf(ret):
      yield ret
