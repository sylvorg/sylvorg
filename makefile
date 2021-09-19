.RECIPEPREFIX := |
.DEFAULT_GOAL := super-push

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
emkDir := home/siluam/
emkFile := $(emkDir)/makefile
emkMake := make -f $(emkFile)
makefly := make -f $(mkfileDir)/makefly.mk

init: pre-init tangle
|$(makefly) pre-init
|$(emkMake) pre-init

pre-init:
|git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/shadowrylander/settings.git
|git -C $(mkfileDir)/settings checkout main
|-git -C $(mkfileDir) config include.path "$(mkfileDir)/.gitconfig"

tangle-setup:
|$(emkMake) tangle-setup

tangle:
|$(emkMake) tangle
|yes yes | fd . $(mkfileDir) \
    -HIe org \
    -E home/siluam \
    -E home/.emacs.d \
    -x settings/backup-tangle.sh
|fd . $(mkfileDir) \
    -HId 1 -e py \
    -x chmod +x

subinit: init
|$(emkMake) subinit
|git -C $(mkfileDir) submodule update --init --depth 1 --recursive
|git -C $(mkfileDir) submodule sync

pull: subinit
|$(emkMake) pull
|git -C $(mkfileDir) pull

add: pre-init
|$(emkMake) add
|git -C $(mkfileDir) add .

commit: pre-init
|$(emkMake) commit
|-git -C $(mkfileDir) commit --allow-empty-message -am ""

cammit: add commit

push: cammit
|$(emkMake) push
|-git -C $(mkfileDir) push

emacs:
|$(emkMake) emacs
emacs-nw:
|$(emkMake) emacs-nw
super-push: tangle push
