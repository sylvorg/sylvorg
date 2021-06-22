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

tangle-damascus: tangle-setup
|yes yes | ~/.emacs.d/org-tangle ~/shadowrylander/damascus.aiern.org

tangle-libs: tangle-setup
|yes yes | find ~/shadowrylander/home/.emacs.d/lib -name README.org -exec ~/.emacs.d/org-tangle "{}" \;

tangle-all: tangle tangle-libs

setup: tangle-damascus copy

test:
|emacs ~/shadowrylander/damascus.aiern.org

emacs: setup test
