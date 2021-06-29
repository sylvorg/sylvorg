emacs ?= emacs

LOAD = -l lv.el -l deino.el -l deino-test.el

.PHONY: all test clean

all: test

test:
	@echo "Using $(shell which $(emacs))..."
	$(emacs) -batch $(LOAD) -f ert-run-tests-batch-and-exit

run:
	$(emacs) -q $(LOAD) -l targets/deino-init.el
	make clean

compile:
	$(emacs) -batch $(LOAD) -l targets/deino-init.el

clean:
	rm -f *.elc
