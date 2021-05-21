.RECIPEPREFIX := |

init:
|-git clone --recurse-submodule https://github.com/shadowrylander/shadowrylander home/shadowrylander
.DEFAULT_GOAL := init

rebuild:
|chmod +x ./wheee
|./wheee --use-hash ${HASH} -H make

switch:
|chmod +x ./wheee
|./wheee --use-hash ${HMASH} -H make --home-manager
|./wheee --use-hash ${RMASH} -H make --home-manager

doom-set:
|rsync -avvczz --delete ~/shadowrylander/home/.doom.d/ ~/.doom.d/
|chmod +x ~/.doom.d/org-tangle
|~/.doom.d/org-tangle ~/shadowrylander/doom.aiern.org
|rsync -avvczz --delete ~/shadowrylander/home/.doom.d/ ~/.doom.d/

doom-sync-check:
|~/.emacs.d/bin/doom sync
|~/.emacs.d/bin/doom doctor

doom-test:
|emacs ~/shadowrylander/promethean.aiern.org

doom: doom-set doom-sync-check doom-test

tangle-all: doom-set
|echo yes | ~/.doom.d/org-tangle ~/shadowrylander/*.org

push-all:
|git add .
|git commit --allow-empty-message -am ""
|git push

super-push: tangle-all push-all
