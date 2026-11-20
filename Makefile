# `make check' is what every commit must pass: the code compiles without
# a warning, is indented as Emacs indents it, and passes its tests.

EMACS ?= emacs

.PHONY: check
check:
	$(EMACS) --batch -Q -L . -l tools/check.el $(wildcard syringe.el test/*.el)
