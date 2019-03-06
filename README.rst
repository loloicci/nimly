#######
 nimly
#######
A lexer generator and the parser generator work in Nim-lang.

niml
====
niml is a lexer generator work in Nim-lang.

macro niml
----------
macro niml makes a lexer.
Example is as follows.

.. code-block:: nim

  niml myLexer:
    "if":
      ## this part converted to procbody.
      ## the arg is (token: LToken).
      return TokenIf()
    "else":
      return TokenElse()
    "true":
      return TokenTrue()
    "false":
      return TokenFalse()
    ## you can use ``..`` instead of ``-`` in ``[]``.
    "[a..zA..Z-_][a..zA..Z0..9\-_]*":
      return TokenIdentifier(token)

This makes a LexData object named myLexer.

Meta charactors are as following:

- ``\``: escape character
- ``.``: match with any charactor
- ``[``: start of character class
- ``]``: end of character class
- ``|``: means or
- ``(``: start of subpattern
- ``)``: end of subpattern
- ``?``: 0 or 1 times quantifier
- ``*``: 0 or more times quantifire
- ``+``: 1 or more times quantifire
- ``{``: ``{n,m}`` is n or more and m or less times quantifire

In ``[]``, meta charactors are as following

- ``\``: escape character
- ``^``: negate character (only in first position)
- ``]``: end of class
- ``-``: specify character range (``..`` can be used instead of this)

Each of followings is recognized as character set.

- ``\d``: ``[0..9]``
- ``\D``: ``[^0..9]``
- ``\s``: ``[ \t\n\r\f\v]``
- ``\S``: ``[^ \t\n\r\f\v]``
- ``\w``: ``[a..zA..Z0..9_]``
- ``\w``: ``[^a..zA..Z0..9_]``
