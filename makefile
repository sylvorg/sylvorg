.RECIPEPREFIX := |
.DEFAULT_GOAL := super-push

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
emkDir := home/.emacs.d/
emkFile := $(emkDir)/makefile
emkMake := make -f $(emkFile)

init:
|$(emkMake) init
|-git -C $(mkfileDir) config include.path "$(mkfileDir)/.gitconfig"

subinit:
|git -C $(mkfileDir) submodule update --init --depth 1 --recursive
|git -C $(mkfileDir) submodule sync
|$(emkMake) subinit

pull: init subinit
|$(emkMake) pull
|git -C $(mkfileDir) pull

add: init
|$(emkMake) add
|git -C $(mkfileDir) add .

commit: init
|$(emkMake) commit
|-git -C $(mkfileDir) commit --allow-empty-message -am ""

cammit: add commit

push: cammit
|$(emkMake) push
|-git -C $(mkfileDir) push

tangle-setup:
|$(emkMake) tangle-setup

tangle:
|$(emkMake) tangle
|yes yes | fd . $(mkfileDir) \
    -HIe org \
    -E home/.emacs.d \
    -x home/.emacs.d/backup-tangle.sh
|fd . $(mkfileDir) \
    -HId 1 -e py \
    -x chmod +x

emacs:
|$(emkMake) emacs
emacs-nw:
|$(emkMake) emacs-nw
super-push: tangle push
