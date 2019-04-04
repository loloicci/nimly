import patty
import nimly
import parser_415

nimy psr415LR[MyTerm, LR0]:
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
