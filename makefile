.RECIPEPREFIX := |
.DEFAULT_GOAL := tangle

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
emkDir := $(mkfileDir)/.emacs.d/
emkFile := $(emkDir)/makefile
emkMake := make -f $(emkFile)

README:
|yes yes | $(mkfileDir)/settings/org-tangle.sh $(mkfileDir)/README.org

both: README
|yes yes | $(mkfileDir)/settings/org-tangle.sh $(mkfileDir)/oreo.aiern.org

tangle: both
|chmod +x $(mkfileDir)/mscripts/tangle
|$(mkfileDir)/mscripts/tangle $(mkfileDir)

tangle-emacs:
|$(emkMake) tangle

tangle-all: tangle tangle-emacs

subinit:
|$(emkMake) subinit
|yadm submodule sync --recursive

emacs:
|$(emkMake) emacs
emacs-nw:
|$(emkMake) emacs-nw

stow:
|chmod +x $(mkfileDir)/mscripts/stow
|$(mkfileDir)/mscripts/stow $(mkfileDir)
