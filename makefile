.RECIPEPREFIX := |
.DEFAULT_GOAL := super-push

init:
|sudo cp git-subtree.sh $$(git --exec-path)/git-subtree

rebuild:
|chmod +x ./wheee
|./wheee --use-hash ${HASH} -H make

switch:
|chmod +x ./wheee
|./wheee --use-hash ${HMASH} -H make --home-manager
|./wheee --use-hash ${RMASH} -H make --home-manager

pull:
|git -C ~/shadowrylander pull

push:
|git -C ~/shadowrylander add .
|-git -C ~/shadowrylander commit --allow-empty-message -am ""
|-git -C ~/shadowrylander push

tangle-all:
|make -f ~/shadowrylander/home/.emacs.d/makefile tangle-all

push-emacs:
|make -f ~/shadowrylander/home/.emacs.d/makefile push

emacs:
|make -f ~/shadowrylander/home/.emacs.d/makefile emacs

emacs-test:
|make -f ~/shadowrylander/home/.emacs.d/makefile emacs-test

supermacs:
|make -f ~/shadowrylander/home/.emacs.d/makefile supermacs

super-push: tangle-all push push-emacs
