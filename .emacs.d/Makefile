SHELL = /bin/sh
EMACS ?= emacs

all: elpa

elpa: packages.el
	$(EMACS) -batch -l packages.el

.PHONY: elpa~
