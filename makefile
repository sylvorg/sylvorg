.RECIPEPREFIX := |
.DEFAULT_GOAL := super-push

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
emkDir := home/.emacs.d/
emkFile := $(emkDir)/makefile
emkMake := make -f $(emkFile)

init: pre-init tangle
|$(emkMake) pre-init

pre-init:
|-git -C $(mkfileDir) config include.path "$(mkfileDir)/.gitconfig"
|git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/shadowrylander/.emacs.d.git
|git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/shadowrylander/siluam.git home/.emacs.d

tangle-setup:
|$(emkMake) tangle-setup

tangle:
|$(emkMake) tangle
|yes yes | fd . $(mkfileDir) \
    -HIe org \
    -E .emacs.d \
    -E home/.emacs.d \
    -x settings/backup-tangle.sh
|fd . $(mkfileDir) \
    -HId 1 -e sh \
    -x chmod +x

subinit: init
|$(emkMake) subinit

|-git rm -rf --cached home/resources home/.vim home/.config/neovim
|git -C $(mkfileDir) submodule sync --recursive

# Adapted From:
# Answer: https://stackoverflow.com/a/56621295/10827766
# User: https://stackoverflow.com/users/1600536/alim-giray-aytar
|git -C $(mkfileDir) submodule update --force --init --depth 1 --recursive --remote

|git -C $(mkfileDir) submodule sync --recursive
|-git rm -rf --cached home/resources

pull: subinit
|git -C $(mkfileDir) pull

add: pre-init
|git -C $(mkfileDir) add .

commit: pre-init
|-git -C $(mkfileDir) commit --allow-empty-message -am ""

cammit: add commit

push: cammit
|-git -C $(mkfileDir) push

emacs:
|$(emkMake) emacs
emacs-nw:
|$(emkMake) emacs-nw
super-push: tangle push
