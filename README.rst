#######
 nimly
#######
.. image:: https://dev.azure.com/oxisccl/nimly/_apis/build/status/loloiccl.nimly?branchName=master
    :target: https://dev.azure.com/oxisccl/nimly/_build/latest?definitionId=1&branchName=master
A lexer generator and the parser generator work in Nim-lang.

niml
====
niml is a lexer generator work in Nim-lang.

macro niml
----------
macro niml makes a lexer.
Almost all part of constructing a lexer is done in comile-time.
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
    r"[a..zA..Z\-_][a..zA..Z0..9\-_]*":
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
niml is a LALR(1) parser generator work in Nim-lang.

macro nimy
----------
macro nimy makes a parser.
Almost all part of constructing a parser is done in comile-time.
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
``tests/test_nimly.nim`` is an easy example.

.. code-block:: nim

  import unittest
  import patty
  import strutils

  import nimly

  variant MyToken:
    PLUS
    MULTI
    NUM(val: int)
    IGNORE

  niml testLex[MyToken]:
    r"\+":
      return PLUS()
    r"\*":
      return MULTI()
    r"\d*":
      return NUM(parseInt(token.token))
    r"\s":
      return IGNORE()

  nimy testPar[MyToken]:
    top[string]:
      plus:
        return $1
    plus[string]:
      plus PLUS plus:
        return $1 & " + " & $3
      mult:
        return $1
    mult[string]:
      mult MULTI mult:
        return "(" & $1 & " * " & $3 & ")"
      num:
        return $1
    num[string]:
      NUM:
        return $(($1).val)

  test "test 1":
    var testLexer = testLex.newWithString("1 + 2 * 3")
    testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
    var
      ret: seq[MyTokenKind] = @[]
    for token in testLexer.lexIter:
      ret.add(token.kind)
    check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                   MyTokenKind.MULTI, MyTokenKind.NUM]

  test "test 2":
    var testLexer = testLex.newWithString("1 + 2 * 3")
    testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE
    testPar.initParser()
    check testPar.parse(testLexer) == "1 + (2 * 3)"
    testLexer.initWithString("1 + 2 * 3")
    testPar.initParser()
    check testPar.parse(testLexer) == "1 + (2 * 3)"

Install
=======
1. Clone this repository
2. ``nimble install``

Now, you can use nimly with ``import nimly``.

Contribute
==========
1. Fork this
2. Create new branch
3. Commit your change
4. Push it to the branch
5. Create new pull request

Developing
==========
You can use ``nimldebug`` and ``nimydebug`` as a conditional symbol
to print debug info.

example: ``nim c -d:nimldebug -d:nimydebug -r tests/test_nimly.nim``
