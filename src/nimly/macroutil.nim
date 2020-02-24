import macros

proc debugEchoNode*(debugTag: string, message: string): NimNode {.compiletime.} =
  let
    tag = newIdentNode(debugTag)
    msg = newLit(message)

  result = quote do:
    when defined(`tag`):
      echo `msg`
