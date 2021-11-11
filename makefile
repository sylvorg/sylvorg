.RECIPEPREFIX := |
.DEFAULT_GOAL := tangle

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
emkDir := $(mkfileDir)/.emacs.d/
emkFile := $(emkDir)/makefile
emkMake := make -f $(emkFile)

bootstrap:
|nix-channel --add https://github.com/nixos/nixpkgs/archive/master/nixpkgs.tar.gz master
|nix-channel --update
|nix-env -iA master.emacs master.python310
|-git -C $(mkfileDir)/settings checkout main
|$(mkfileDir)/settings/org-tangle.sh oreo.aiern.org README.org
|chmod +x $(mkfileDir)/wheee.py $(mkfileDir)/bootstrap.py
|-git clone https://github.com/shadowrylander/shadowrylander $(mkfileDir)/shadowrylander

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
