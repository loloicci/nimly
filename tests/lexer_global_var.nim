import strutils

import patty
import nimly

variantp Token:
  Indent(level: int)
  Num(str: string)

var indStack* {.global.}: seq[int] = @[]

niml lexGlobalVar[Token]:
  setUp:
    indStack = @[0]
  "[0-9]+":
    Num(token.token)
  "\n+ *":
    let spaces = token.token.count(' ')
    if indStack[^1] < spaces:
      indStack.add(spaces)
      return Indent(indStack.len - 1)
    else:
      var top = indStack.pop()
      while top > spaces:
        top = indStack.pop()
      indStack.add(top)
      if top == spaces:
        return Indent(indStack.len - 1)
      else:
        indStack.add(spaces)
        return Indent(indStack.len - 1)
