import tables
import patty

import lextypes


proc `~`[A](a: A): ref A =
  new(result)
  result[] = a


type
  Pos = int16
  BOp = enum
    bor,
    bcat
  Pos2PosSet = TableRef[Pos, set[Pos]]

variant LChar:
  End
  Real(c: char)

variant Lit:
  Empty
  Char(pos: Pos, c: LChar)

variant ReSynTree:
  Term(lit: Lit)
  Bin(op: BOp, left: ref ReSynTree, right: ref ReSynTree)
  Star(child: ref ReSynTree)


proc collectPos(t: ReSynTree): set[Pos] =
  # error on tree with not unique positions when debug
  match t:
    Term(lit: l):
      match l:
        Empty:
          return {}
        Char(pos: p, c: _):
          return {p}
    Bin(op: _, left: l, right: r):
      let
        lr = l[].collectPos
        rr = r[].collectPos
      assert lr * rr == {}
      return lr + rr
    Star(child: c):
      return c[].collectPos

proc nullable(t: ReSynTree): bool =
  match t:
    Term(lit: l):
      match l:
        Empty:
          return true
        Char:
          return false
    Bin(op: o, left: l, right: r):
      case o
      of bor:
        return l[].nullable or r[].nullable
      of bcat:
        return l[].nullable and r[].nullable
    Star:
      return true

proc firstpos(t: ReSynTree): set[Pos] =
  match t:
    Term(lit: l):
      match l:
        Empty:
          return {}
        Char(pos: p, c: _):
          return {p}
    Bin(op: o, left: l, right: r):
      case o
      of bor:
        return l[].firstpos + r[].firstpos
      of bcat:
        if l[].nullable:
          return l[].firstpos + r[].firstpos
        else:
          return l[].firstpos
    Star(child: c):
      return c[].firstpos

proc lastpos(t: ReSynTree): set[Pos] =
  match t:
    Term(lit: l):
      match l:
        Empty:
          return {}
        Char(pos: p, c: _):
          return {p}
    Bin(op: o, left: l, right: r):
      case o
      of bor:
        return l[].lastpos + r[].lastpos
      of bcat:
        if r[].nullable:
          return l[].lastpos + r[].lastpos
        else:
          return r[].lastpos
    Star(child: c):
      return c[].lastpos

proc mergeSetTable[A; B](a: var TableRef[A, set[B]], b: TableRef[A, set[B]]) =
  for k in b.keys:
    a[k] = a.getOrDefault(k) + b[k]

proc makeFollowposTable(t: ReSynTree): Pos2PosSet =
  # init
  result = newTable[Pos, set[Pos]]()

  # make
  match t:
    Term:
      return
    Bin(op: o, left: l, right: r):
      if o == bcat:
        for i in l[].lastpos:
          result[i] = r[].firstpos
      result.mergeSetTable(l[].makeFollowposTable)
      result.mergeSetTable(r[].makeFollowposTable)
    Star(child: c):
      for i in t.lastpos:
        result[i] = t.firstpos
      result.mergeSetTable(c[].makeFollowposTable)
