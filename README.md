minikanren
==========

minikanren (logic programming language) for guile.

You need to point the GUILE_LOAD_PATH to this directory to load this code.

To test the cd into tests/ and make test.

To get started inside a guile repl load the module (use-modules (minikanren language))

Editing
=======

To get emacs to indent minikanren code correctly add to .emacs:

(put 'fresh 'scheme-indent-function 1)
