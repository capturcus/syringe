# `make check' is what every commit must pass: syringe.el compiles
# without a warning and is indented as Emacs indents it.

EMACS ?= emacs

.PHONY: check
check:
	$(EMACS) --batch -Q -l tools/check.el $(wildcard syringe.el)
