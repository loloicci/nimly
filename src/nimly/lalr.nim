import tables
import sets
import hashes

import patty

import parsetypes
import parser
import lr

proc makeTableLALR*[T](g: Grammar[T]): ParsingTable[T] =
  discard
