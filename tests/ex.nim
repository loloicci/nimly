import nimly
import patty

variant Token:
  FOO

niml a[Token]:
  r"\:":
    return FOO()
