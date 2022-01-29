.RECIPEPREFIX := |
.DEFAULT_GOAL := tangle

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
emkDir := $(mkfileDir)/.emacs.d/
emkFile := $(emkDir)/makefile
emkMake := make -f $(emkFile)

README:
|git tangle $(mkfileDir)/README.org

tangle: README
|git tangle $(mkfileDir)/oreo.aiern.org

tangle-emacs:
|$(emkMake) tangle

tangle-all: tangle tangle-emacs

emacs:
|$(emkMake) emacs
emacs-nw:
|$(emkMake) emacs-nw
