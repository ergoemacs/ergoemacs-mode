CASK ?= cask
EMACS ?= emacs

all: test

test:
	cask exec ert-runner ergoemacs-test.el

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile ansi.el

clean-elc:
	rm -f ansi.elc

.PHONY: all commander test clean-elc
