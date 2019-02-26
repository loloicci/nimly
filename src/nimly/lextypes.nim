type
  State* = int16
  CInt* = int8
  StateSet* = set[State]

  DBRow* = object
    default*: State
    base*: CInt

  NCRow* = object
    next*: State
    check*: State
