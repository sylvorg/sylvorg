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

doom-test:
|emacs ~/shadowrylander/promethean.aiern.org

doom-set:
|chmod +x ~/.doom.d/org-tangle
|~/.doom.d/org-tangle ~/shadowrylander/doom.aiern.org
|rsync -avvczz --delete ~/shadowrylander/home/.doom.d/ ~/.doom.d/

doom-both: doom-set doom-test

tangle-all:
|chmod +x ~/.doom.d/org-tangle
|~/.doom.d/org-tangle ~/shadowrylander/*.org

# remake:
