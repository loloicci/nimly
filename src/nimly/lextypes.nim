type
  State = int
  CInt = int

  Defaults = seq[State]
  Bases = seq[CInt]
  Nexts = seq[State]
  Checks = seq[State]
  Accepts[A] = seq[proc(s: string): A]
