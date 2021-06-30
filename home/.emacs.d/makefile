.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))

init:
|-sudo cp $(mkfileDir)/git-subtree $$(git --exec-path)/

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
|chmod +x $(mkfileDir)/org-tangle

tangle: tangle-setup
|-ln -sf $$(which fdfind) ~/.local/bin/fd
|yes yes | fd . $(mkfileDir) \
    -HIe org \
    -E $(mkfileDir)/yankpad.org \
    -x $(mkfileDir)/org-tangle

test:
|emacs $(mkfileDir)/testing.aiern.org

test-and-kill:
|-emacsclient -s test -e "(kill-emacs)"
|emacs $(mkfileDir)/testing.aiern.org --bg-daemon=test
|emacsclient -s test -e "(kill-emacs)"

emacs: tangle test
