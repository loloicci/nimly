import patty
import nimly
import strutils

variantp StateToken:
  SPLUS
  SMULTI
  SNUM(val: int)
  SID(str: string)
  SLPAREN
  SRPAREN
  SLBRACE
  SRBRACE
  SIF
  SELSE
  STHEN
  SIGNORE

niml testStateLex[StateToken]:
  r"\+":
    return SPLUS()
  r"\*":
    return SMULTI()
  r"\d+":
    return SNUM(parseInt(token.token))
  r"if":
    return SIF()
  r"else":
    return SELSE()
  r"then":
    return STHEN()
  r"\(":
    return SLPAREN()
  r"\)":
    return SRPAREN()
  r"\{":
    return SLBRACE()
  r"}":
    return SRBRACE()
  r"[a..zA..Z_]\w*":
    return SID(token.token)
  r"\s":
    return SIGNORE()

nimy testStatePar[StateToken]:
  top[string]:
    state:
      return $1
  state[string]:
    SIF exp STHEN SLBRACE state SRBRACE SELSE SLBRACE state SRBRACE:
      return "IF(" & $2 & ")THEN{" & $5 & "}ELSE{" & $9 & "}"
    exp:
      return $1
  exp[string]:
    plus:
      return $1
  plus[string]:
    plus SPLUS plus:
      return $1 & "+" & $3
    mult:
      return $1
  mult[string]:
    mult SMULTI mult:
      return "(" & $1 & "*" & $3 & ")"
    num:
      return $1
  num[string]:
    SNUM:
      return $(($1).val)
    SID:
      return $(($1).str)
    SLPAREN exp SRPAREN:
      return "(" & $2 & ")"
