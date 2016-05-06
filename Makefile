emacs ?= emacs

all: compile test

compile:
	cask exec $(emacs) -Q --batch -L . -f batch-byte-compile number-to-word.el

test:
	@echo "Using $(shell which $(emacs))..."
	cask exec $(emacs) -Q --batch -L . -l number-to-word-tests -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
