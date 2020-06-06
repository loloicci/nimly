#######
 nimly
#######
|github_workflow| |azure_pipelines| |nimble|

Lexer Generator and Parser Generator as a Macro Library in Nim.

With nimly, you can make lexer/parser by writing definition
in formats like lex/yacc.
``nimly`` generates lexer and parser by using macro in compile-time,
so you can use ``nimly`` not as external tool of your program but as a library.

niml
====
``niml`` is a lexer generator work in Nim-lang.

macro niml
----------
macro ``niml`` makes a lexer.
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
``nimy`` is a LALR(1) parser generator work in Nim-lang.

macro nimy
----------
macro ``nimy`` makes a parser.
Almost all part of constructing a parser is done in compile-time.
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

You can use following EBNF functions:

- ``XXX[]``: Option (0 or 1 ``XXX``).
  The type is ``seq[xxx]`` where ``xxx`` is type of ``XXX``.
- ``XXX{}``: Repeat (0 or more ``XXX``).
  The type is ``seq[xxx]`` where ``xxx`` is type of ``XXX``.

Example of these is in next section.

Example
=======
``tests/test_readme_example.nim`` is an easy example.

.. code-block:: nim

  import unittest
  import patty
  import strutils

  import nimly

  ## variant is defined in patty
  variant MyToken:
    PLUS
    MULTI
    NUM(val: int)
    DOT
    LPAREN
    RPAREN
    IGNORE

  niml testLex[MyToken]:
    r"\(":
      return LPAREN()

    r"\)":
      return RPAREN()

    r"\+":
      return PLUS()

    r"\*":
      return MULTI()

    r"\d":
      return NUM(parseInt(token.token))

    r"\.":
      return DOT()

    r"\s":
      return IGNORE()


  nimy testPar[MyToken]:
    top[string]:
      plus:
        return $1

    plus[string]:
      mult PLUS plus:
        return $1 & " + " & $3

      mult:
        return $1

    mult[string]:
      num MULTI mult:
        return "[" & $1 & " * " & $3 & "]"

      num:
        return $1

    num[string]:
      LPAREN plus RPAREN:
        return "(" & $2 & ")"

      ## float (integer part is 0-9) or integer
      NUM DOT[] NUM{}:
        result = ""

        # type of `($1).val` is `int`
        result &= $(($1).val)

        if ($2).len > 0:
          result &= "."

        # type of `$3` is `seq[MyToken]` and each elements are NUM
        for tkn in $3:
          # type of `tkn.val` is `int`
          result &= $(tkn.val)


  test "test Lexer":
    var testLexer = testLex.newWithString("1 + 42 * 101010")
    testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

    var
      ret: seq[MyTokenKind] = @[]

    for token in testLexer.lexIter:
      ret.add(token.kind)

    check ret == @[MyTokenKind.NUM, MyTokenKind.PLUS, MyTokenKind.NUM,
                   MyTokenKind.NUM, MyTokenKind.MULTI,
                   MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM,
                   MyTokenKind.NUM, MyTokenKind.NUM, MyTokenKind.NUM]


  test "test Parser 1":
    var testLexer = testLex.newWithString("1 + 42 * 101010")
    testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

    testPar.init()
    check testPar.parse(testLexer) == "1 + [42 * 101010]"

    testLexer.initWithString("1 + 42 * 1010")

    testPar.init()
    check testPar.parse(testLexer) == "1 + [42 * 1010]"


  test "test Parser 2":
    var testLexer = testLex.newWithString("1 + 42 * 1.01010")
    testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

    testPar.init()
    check testPar.parse(testLexer) == "1 + [42 * 1.01010]"

    testLexer.initWithString("1. + 4.2 * 101010")

    testPar.init()
    check testPar.parse(testLexer) == "1. + [4.2 * 101010]"


  test "test Parser 3":
    var testLexer = testLex.newWithString("(1 + 42) * 1.01010")
    testLexer.ignoreIf = proc(r: MyToken): bool = r.kind == MyTokenKind.IGNORE

    testPar.init()
    check testPar.parse(testLexer) == "[(1 + 42) * 1.01010]"


Install
=======
1. ``nimble install nimly``

Now, you can use nimly with ``import nimly``.

vmdef.MaxLoopIterations Problem
-------------------------------
See https://github.com/loloiccl/nimly/issues/11

Contribute
==========
1. Fork this
2. Create new branch
3. Commit your change
4. Push it to the branch
5. Create new pull request

Changelog
=========
See changelog.rst_.

Developing
==========
You can use ``nimldebug`` and ``nimydebug`` as a conditional symbol
to print debug info.

example: ``nim c -d:nimldebug -d:nimydebug -r tests/test_readme_example.nim``


.. |github_workflow| image:: https://github.com/loloicci/nimly/workflows/test/badge.svg
    :target: https://github.com/loloicci/nimly/actions?query=workflow%3Atest
.. |azure_pipelines| image:: https://dev.azure.com/oxisccl/nimly/_apis/build/status/loloicci.nimly?branchName=master
    :target: https://dev.azure.com/oxisccl/nimly/_build/latest?definitionId=1&branchName=master
.. |nimble| image:: https://raw.githubusercontent.com/yglukhov/nimble-tag/master/nimble.png
    :target: https://github.com/yglukhov/nimble-tag
.. _changelog.rst: ./changelog.rst
