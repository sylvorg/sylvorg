SHELL = /bin/sh
EMACS ?= emacs
FILES = $(filter-out aiern-test-helpers.el aiern-tests.el aiern-pkg.el,$(wildcard aiern*.el))
VERSION := $(shell sed -ne '/define-package/,$$p' aiern-pkg.el | sed -ne '/^\s*"[[:digit:]]\+\(\.[[:digit:]]\+\)*"\s*$$/ s/^.*"\(.*\)".*$$/\1/p')
ELPAPKG = aiern-$(VERSION)
PROFILER =
DOC = doc
TAG =
LIBS = -L lib

ELCFILES = $(FILES:.el=.elc)

.PHONY: all compile compile-batch docstrings doc clean tests test emacs term terminal profiler indent elpa version

# Byte-compile aiern.
all: compile
compile: $(ELCFILES)

.depend: $(FILES)
	@echo Compute dependencies
	@rm -f .depend
	@for f in $(FILES); do \
		sed -n "s/ *(require '\(aiern-[^)]*\).*)/$${f}c: \1.elc/p" $$f >> .depend;\
	done

-include .depend

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<

# Byte-compile all files in one batch. This is faster than
# compiling each file in isolation, but also less stringent.
compile-batch: clean
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile ${FILES}

# Documentation.
docstrings:
	@$(EMACS) --script scripts/aiern-extract-docstrings

doc: docstrings
	@$(MAKE) -C doc texinfo

info: doc
	@$(MAKE) -C doc info

# Delete byte-compiled files etc.
clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc
	rm -f .depend
	@$(MAKE) -C doc clean

# Run tests.
# The TAG variable may specify a test tag or a test name:
#       make test TAG=repeat
# This will only run tests pertaining to the repeat system.
test:
	$(EMACS) -nw -Q -L . $(LIBS) -l aiern-tests.el \
		--eval "(aiern-tests-initialize '(${TAG}) '(${PROFILER}))"

# Byte-compile aiern and run all tests.
tests: compile
	$(EMACS) -nw -Q -L . $(LIBS) -l aiern-tests.el \
		--eval "(aiern-tests-initialize '(${TAG}) '(${PROFILER}))"
	rm -f *.elc .depend

# Load aiern in a fresh instance of Emacs and run all tests.
emacs:
	$(EMACS) -Q -L . $(LIBS) -l goto-chg.el -l aiern-tests.el \
		--eval "(aiern-mode 1)" \
		--eval "(aiern-tests-initialize '(${TAG}) '(${PROFILER}) t)"

# Load aiern in a terminal Emacs and run all tests.
term: terminal
terminal:
	$(EMACS) -nw -Q -L . $(LIBS) -l goto-chg.el -l aiern-tests.el \
		--eval "(aiern-mode 1)" \
		--eval "(aiern-tests-initialize '(${TAG}) '(${PROFILER}) t)"

# Run all tests with profiler.
profiler:
	$(EMACS) --batch -Q -L . $(LIBS) -l goto-chg.el -l aiern-tests.el \
		--eval "(aiern-tests-initialize '(${TAG}) (or '(${PROFILER}) t))"

# Re-indent all aiern code.
# Loads aiern into memory in order to indent macros properly.
# Also removes trailing whitespace, tabs and extraneous blank lines.
indent: clean
	$(EMACS) --batch --eval '(setq vc-handled-backends nil)' ${FILES} aiern-tests.el -Q -L . $(LIBS) -l aiern-tests.el \
		--eval "(dolist (buffer (reverse (buffer-list))) \
		(when (buffer-file-name buffer) \
		(set-buffer buffer) \
		(message \"Indenting %s\" (current-buffer)) \
		(setq-default indent-tabs-mode nil) \
		(untabify (point-min) (point-max)) \
		(indent-region (point-min) (point-max)) \
		(delete-trailing-whitespace) \
		(untabify (point-min) (point-max)) \
		(goto-char (point-min)) \
		(while (re-search-forward \"\\n\\\\{3,\\\\}\" nil t) \
		(replace-match \"\\n\\n\")) \
		(when (buffer-modified-p) (save-buffer 0))))"

# Create an ELPA package.
elpa:
	@echo "Creating ELPA package $(ELPAPKG).tar"
	@rm -rf ${ELPAPKG}
	@mkdir ${ELPAPKG}
	@cp $(FILES) COPYING aiern-pkg.el ${ELPAPKG}
	@tar cf ${ELPAPKG}.tar ${ELPAPKG}
	@rm -rf ${ELPAPKG}

# Change the version using make VERSION=x.y.z
version:
	@$(EMACS) --script scripts/aiernupdate "${VERSION}"

# Change the version using make VERSION=x.y.z, but do not post to the newsgroup
nversion:
	@$(EMACS) --script scripts/aiernupdate nonews "${VERSION}"

