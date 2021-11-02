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
|fd . $(mkfileDir) \
    -HId 1 -e sh \
    -x chmod +x

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

stow: SHELL := /usr/bin/env xonsh
stow:
|-chmod 700 $(mkfileDir)/.ssh
|chmod +x $(mkfileDir)/run_keybase
|$(mkfileDir)/run_keybase login
|git -C $(mkfileDir)/keybase/secrets pull
|-for f in $$(ls -a $(mkfileDir)/keybase/secrets/.ssh).split(): \
    rm $(mkfileDir)/.ssh/@(f)
|stow -d $(mkfileDir)/keybase -t $(mkfileDir) secrets
|chmod 700 $(mkfileDir)/.ssh
|chmod 600 $(mkfileDir)/.ssh/*
