import patty

type
  State = int
  CInt = int

  # token
  LToken = object
    token: string
    colNum: int
    lineNum: int
    lineInfo: string

variant Accept[T]:
  NotAcc
  Acc(fun: proc(token: LToken): T {.nimcall.})

type
  DBA[T] = object
    default: State
    base: CInt
    accept: Accept[T]

variant NC:
  EmptyRow
  DataRow(next: State, check: State)

type
  DBATable[T] = seq[DBA[T]]
  NCTable = seq[NC]
  LexData[T] = object
    dba: DBATable[T]
    nc: NCTable
