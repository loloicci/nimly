import patty

type
  State* = int
  CInt* = int

  # token
  LToken* = object
    token*: string
    colNum*: int
    lineNum*: int
    lineInfo*: string

type AccProc*[T] = proc(token: LToken): T {.nimcall.}

variantp Accept[T]:
  NotAcc
  Acc(fun: AccProc[T])

type
  DBA*[T] = object
    default*: State
    base*: CInt
    accept*: Accept[T]

variantp NC:
  EmptyRow
  DataRow(next: State, check: State)

type
  DBATable*[T] = seq[DBA[T]]
  NCTable* = seq[NC]
  LexData*[T] = object
    dba*: DBATable[T]
    nc*: NCTable

const
  deadState* = -1
