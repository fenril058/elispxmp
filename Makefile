all:

TESTFILE := elispxmp-test.el

EMACS    ?= emacs
ELS      := $(shell cask files)

##############################

.PHONY: all build test clean

all: build

build: $(ELS:%.el=%.elc)

%.elc: %.el .cask
	cask exec $(EMACS) -Q --batch -f batch-byte-compile $<

.cask: Cask
	cask install
	touch $@

test: build
	cask exec $(EMACS) -Q --batch -l $(TESTFILE) -f cort-test-run

clean:
	cask clean-elc
	rm -rf .cask
