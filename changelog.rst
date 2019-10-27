###########
 Changelog
###########

[v0.4.0] - 2019-09-25
=====================

Changed
-------
* Update depended ``nim`` version.

  * Change not to print debug massage in non-debug bulid.
    (the printing was needed not to issue error in generating parser.
    This relates some nim VM's bugs.)

Fixed
-----
* Avoid to issue some warns in sanity running.


[v0.3.0] - 2019-07-04
=====================

Changed
-------
* Update depended ``nim`` version.

  * Change to use macros.strVal
  * Change to use xxxHashSet instead of xxxHash

[v0.2.1] - 2019-04-12
=====================

Added
-----
* Add some tests.

Fixed
-----
* Fix the bug that ``nimy``'s rule clause cannot contains comments.

[v0.2.0] - 2019-04-11
=====================

Added
-----
* Add ``[]`` and ``{}`` (for EBNF) to ``nimy``'s syntax.

[v0.1.0] - 2019-04-04
=====================

Added
-----
* Add functions to generate LALR(1) parsing table.

Changed
-------
* Change ``nimy`` to make LALR(1) parsing table insted of LR(1) parsing table
  in default.
* Many refactorings.

[v0.0.0] - 2019-03-22
=====================
The first release
