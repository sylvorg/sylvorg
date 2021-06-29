.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs

pull:
|git -C ~/shadowrylander subtree pull-all

push:
|git -C ~/shadowrylander subtree prune
|-git -C ~/shadowrylander subtree push-all

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
|emacs ~/shadowrylander/damascus.aiern.org --bg-daemon=test
|emacsclient -s test -e "(evil-quit t)"

emacs: setup test
emacs-and-kill: setup test-and-kill
supermacs: tangle-all emacs-and-kill emacs-and-kill emacs
libemacs: tangle-libs emacs-and-kill emacs
