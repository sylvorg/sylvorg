.RECIPEPREFIX := |
.DEFAULT_GOAL := super-push

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
emkFile := $(mkfileDir)/home/.emacs.d/makefile

init:
|make -f $(emkFile) init

pull: init
|git -C $(mkfileDir) pull
|git -C $(mkfileDir) subtree pull-all

push: init
|git -C $(mkfileDir) add .
|-git -C $(mkfileDir) commit --allow-empty-message -am ""
|-git -C $(mkfileDir) push
|git -C $(mkfileDir) subtree prune
|-git -C $(mkfileDir) subtree push-all

tangle-setup:
|make -f $(emkFile) tangle-setup

tangle: tangle-setup
|yes yes | fd . $(mkfileDir) \
    -HIe org \
    -E .emacs.d \
    -x $(mkfileDir)/home/.emacs.d/backup-tangle.sh

tangle-with-emacs: tangle
|make -f $(emkFile) tangle

super-push: tangle-with-emacs push
