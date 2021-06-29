.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs

pull:
|git -C ~/shadowrylander/home/.emacs.d pull
|git -C ~/shadowrylander/home/.emacs.d subtree pull-all

push:
|git -C ~/shadowrylander/home/.emacs.d add .
|-git -C ~/shadowrylander/home/.emacs.d commit --allow-empty-message -am ""
|-git -C ~/shadowrylander/home/.emacs.d push
|git -C ~/shadowrylander/home/.emacs.d subtree prune
|-git -C ~/shadowrylander/home/.emacs.d subtree push-all

copy:
|rsync -avvczz ~/shadowrylander/home/.emacs.d/ ~/.emacs.d/

tangle-setup: copy
|chmod +x ~/.emacs.d/org-tangle
|yes yes| ~/.emacs.d/org-tangle ~/shadowrylander/damascus.aiern.org
|make -f ~/shadowrylander/home/.emacs.d/makefile copy

tangle: tangle-setup
|yes yes | ~/.emacs.d/org-tangle ~/shadowrylander/*.aiern.org
|yes yes | ~/.emacs.d/org-tangle ~/shadowrylander/README.org

tangle-damascus: tangle-README
|yes yes | ~/.emacs.d/org-tangle ~/shadowrylander/damascus.aiern.org

tangle-README: tangle-setup
|yes yes | ~/.emacs.d/org-tangle ~/shadowrylander/home/.emacs.d/README.org

tangle-libs: tangle-setup
|yes yes | find ~/shadowrylander/home/.emacs.d/lib -name README.org -exec ~/.emacs.d/org-tangle "{}" \;

tangle-all: tangle tangle-README tangle-libs

setup: tangle-damascus copy

test:
|emacs ~/shadowrylander/damascus.aiern.org

test-and-kill:
|-emacsclient -s test -e "(kill-emacs)"
|emacs ~/shadowrylander/damascus.aiern.org --bg-daemon=test
|emacsclient -s test -e "(kill-emacs)"

emacs: setup test
emacs-and-kill: setup test-and-kill
i-have-to-do-this-twice: emacs-and-kill emacs-and-kill
supermax: tangle-all i-have-to-do-this-twice emacs
libemacs: tangle-libs emacs-and-kill emacs
