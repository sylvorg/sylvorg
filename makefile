.RECIPEPREFIX := |
.DEFAULT_GOAL := push

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
emkDir := ~/.emacs.d/
emkFile := $(emkDir)/makefile
emkMake := make -f $(emkFile)

oreo:
|yes yes | $(mkfileDir)/settings/org-tangle.sh oreo.aiern.org

README:
|yes yes | $(mkfileDir)/settings/org-tangle.sh README.org

both: oreo README

tangle: both
|$(emkMake) tangle
|fd . $(mkfileDir) \
    -HId 1 -e sh \
    -x chmod +x

subinit:
|$(emkMake) subinit
|yadm submodule sync --recursive

pull: subinit
|yadm pull

commit: tangle
|-yadm commit --allow-empty-message -am ""

push: commit
|-yadm push

emacs:
|$(emkMake) emacs
emacs-nw:
|$(emkMake) emacs-nw

stow:
|-chmod 700 ~/.ssh
|git -C ~/keybase/secrets pull
|-rm ~/.ssh/id_rsa ~/.ssh/known_hosts
|stow -d ~/keybase -t ~ secrets
|chmod 700 ~/.ssh
|chmod 600 ~/.ssh/id_rsa ~/.ssh/known_hosts
