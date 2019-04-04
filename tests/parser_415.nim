import patty
import nimly

variantp MyTerm:
  EQ
  ST
  ID(val: string)
  IGNORE

niml lex415[MyTerm]:
  r"=":
    return EQ()
  r"\*":
    return ST()
  r"[a..zA..Z\-_][a..zA..Z0..9\-_]*":
    return ID(token.token)
  r"\s":
    return IGNORE()

nimy psr415LALR[MyTerm]:
  start[string]:
    left EQ right:
      return $1 & "=" & $3
    right:
      return $1
  left[string]:
    ST right:
      return "*" & $2
    ID:
      return ($1).val
  right[string]:
    left:
      return $1
