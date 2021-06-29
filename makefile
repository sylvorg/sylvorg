.RECIPEPREFIX := |
.DEFAULT_GOAL := super-push

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))

init:
|sudo cp git-subtree.sh $$(git --exec-path)/git-subtree

pull:
|git -C $(mkfileDir) pull
|git -C $(mkfileDir) subtree pull-all

push:
|git -C $(mkfileDir) add .
|-git -C $(mkfileDir) commit --allow-empty-message -am ""
|-git -C $(mkfileDir) push
|git -C $(mkfileDir) subtree prune
|-git -C $(mkfileDir) subtree push-all

tangle-setup:
|chmod +x $(mkfileDir)/home/.emacs.d/org-tangle

tangle:
|yes yes | $(mkfileDir)/home/.emacs.d/org-tangle $(mkfileDir)/*.aiern.org
|yes yes | $(mkfileDir)/home/.emacs.d/org-tangle $(mkfileDir)/README.org
|make -f $(mkfileDir)/home/.emacs.d/makefile tangle

super-push: tangle push
