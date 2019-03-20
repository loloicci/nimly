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

  ## This makes a LexData object named myLexer.
  ## This lexer returns value with type ``Token`` when a token is found.
  niml myLexer[Token]:
    r"if":
      ## this part converted to procbody.
      ## the arg is (token: LToken).
      return TokenIf()
    r"else":
      return TokenElse()
    r"true":
      return TokenTrue()
    r"false":
      return TokenFalse()
    ## you can use ``..`` instead of ``-`` in ``[]``.
    r"[a..zA..Z-_][a..zA..Z0..9\-_]*":
      return TokenIdentifier(token)

Meta charactors are as following:

- ``\``: escape character
- ``.``: match with any charactor
- ``[``: start of character class
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
- ``]``: end of this class
- ``-``: specify character range (``..`` can be used instead of this)

Each of followings is recognized as character set.

- ``\d``: ``[0..9]``
- ``\D``: ``[^0..9]``
- ``\s``: ``[ \t\n\r\f\v]``
- ``\S``: ``[^ \t\n\r\f\v]``
- ``\w``: ``[a..zA..Z0..9_]``
- ``\w``: ``[^a..zA..Z0..9_]``

nimy
====
niml is a lr(0) parser generator work in Nim-lang.
lalr(1) version is under developing.

macro nimy
----------
macro nimy makes a parser.
Example is as follows.

.. code-block:: nim

  ## This makes a LexData object named myParser.
  ## first cloud is the top-level of the BNF.
  ## This lexer recieve tokens with type ``Token`` and token must have a value
  ## ``kind`` with type enum ``[TokenTypeName]Kind``.
  ## This is naturally satisfied when you use ``patty`` to define the token.
  nimy myParser[Token]:
    ## the starting non-terminal
    ## the return type of the parser is ``Expr``
    top[Expr]:
      ## a pattern.
      expr:
        ## proc body that is used when parse the pattern with single ``expr``.
        ## $1 means first position of the pattern (expr)
        return $1
    ## non-terminal named ``expr``
    ## with returning type ``Expr``
    expr[Expr]:
      ## first pattern of expr.
      ## ``LPAR`` and ``RPAR`` is TokenKind.
      LPAR expr RPAR:
        return $2
      ## second pattern of expr.
      ## ``PLUS`` is TokenKind.
      expr PLUS expr
        return $2

example
=======
Look ``tests/test_nimly.nim``
