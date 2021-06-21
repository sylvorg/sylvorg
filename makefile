.RECIPEPREFIX := |

init:
|-git clone --recurse-submodule https://github.com/shadowrylander/shadowrylander ~/shadowrylander/home/shadowrylander
.DEFAULT_GOAL := init

rebuild:
|chmod +x ./wheee
|./wheee --use-hash ${HASH} -H make

switch:
|chmod +x ./wheee
|./wheee --use-hash ${HMASH} -H make --home-manager
|./wheee --use-hash ${RMASH} -H make --home-manager

# doom-set:
# |rsync -avvczz --delete ~/shadowrylander/home/.doom.d/ ~/.doom.d/
# |chmod +x ~/.doom.d/org-tangle
# |yes yes| ~/.doom.d/org-tangle ~/shadowrylander/doom.aiern.org
# |yes yes| ~/.doom.d/org-tangle ~/shadowrylander/home/.doom.d/aiern/README.org
# |rsync -avvczz --delete ~/shadowrylander/home/.doom.d/ ~/.doom.d/

# doom-sync:
# |~/.emacs.d/bin/doom sync

# doom-check:
# |~/.emacs.d/bin/doom doctor

# doom-test:
# |emacs ~/shadowrylander/doom.aiern.org

# doom-pre: doom-set doom-sync doom-check

# doom: doom-pre doom-test

# doom-upgrade: doom-pre
# |printf "n\ny\n" | ~/.emacs.d/bin/doom upgrade

# doom-super: doom-set doom-sync doom-check doom-upgrade doom-sync doom-check doom-test

emacs-copy:
|rsync -avvczz ~/shadowrylander/home/.emacs.d/ ~/.emacs.d/

tangle-setup: emacs-copy
|chmod +x ~/.emacs.d/org-tangle
|yes yes| ~/.emacs.d/org-tangle ~/shadowrylander/damascus.aiern.org
|rsync -avvczz ~/shadowrylander/home/.emacs.d/ ~/.emacs.d/

tangle-damascus: tangle-setup
# Adapted From:
# Answer: https://askubuntu.com/a/338860/1058868
# User: https://askubuntu.com/users/1366/lesmana
# From:
# Answer: https://askubuntu.com/a/446480/1058868
# User: https://askubuntu.com/users/267867/peter-w-osel
|yes yes | ~/.emacs.d/org-tangle ~/shadowrylander/damascus.aiern.org

tangle-use-package-extras: tangle-setup
# Adapted From:
# Answer: https://askubuntu.com/a/338860/1058868
# User: https://askubuntu.com/users/1366/lesmana
# From:
# Answer: https://askubuntu.com/a/446480/1058868
# User: https://askubuntu.com/users/267867/peter-w-osel
|yes yes | ~/.emacs.d/org-tangle ~/shadowrylander/home/.emacs.d/lib/use-package-extras/README.org

tangle-alamode: tangle-setup
# Adapted From:
# Answer: https://askubuntu.com/a/338860/1058868
# User: https://askubuntu.com/users/1366/lesmana
# From:
# Answer: https://askubuntu.com/a/446480/1058868
# User: https://askubuntu.com/users/267867/peter-w-osel
|yes yes | ~/.emacs.d/org-tangle ~/shadowrylander/home/.emacs.d/lib/alamode/README.org

tangle: tangle-setup
# Adapted From:
# Answer: https://askubuntu.com/a/338860/1058868
# User: https://askubuntu.com/users/1366/lesmana
# From:
# Answer: https://askubuntu.com/a/446480/1058868
# User: https://askubuntu.com/users/267867/peter-w-osel
|yes yes | ~/.emacs.d/org-tangle ~/shadowrylander/*.aiern.org
|yes yes | ~/.emacs.d/org-tangle ~/shadowrylander/README.org

tangle-all: tangle tangle-use-package-extras tangle-alamode

emacs-setup: tangle-damascus emacs-copy

emacs-test:
|emacs ~/shadowrylander/damascus.aiern.org

emacs: emacs-setup emacs-test

push:
|git -C ~/shadowrylander add .
|-git -C ~/shadowrylander commit --allow-empty-message -am ""
|-git -C ~/shadowrylander push

push-aiern:
|git -C ~/shadowrylander/home/.emacs.d/lib/aiern add .
|-git -C ~/shadowrylander/home/.emacs.d/lib/aiern commit --allow-empty-message -am ""
|-git -C ~/shadowrylander/home/.emacs.d/lib/aiern push

push-doom-aiern-modeline:
|git -C ~/shadowrylander/home/.emacs.d/lib/doom-aiern-modeline add .
|-git -C ~/shadowrylander/home/.emacs.d/lib/doom-aiern-modeline commit --allow-empty-message -am ""
|-git -C ~/shadowrylander/home/.emacs.d/lib/doom-aiern-modeline push

push-use-package-extras:
|git -C ~/shadowrylander/home/.emacs.d/lib/use-package-extras add .
|-git -C ~/shadowrylander/home/.emacs.d/lib/use-package-extras commit --allow-empty-message -am ""
|-git -C ~/shadowrylander/home/.emacs.d/lib/use-package-extras push

push-tag:
|git -C ~/shadowrylander/home/.emacs.d/lib/tag add .
|-git -C ~/shadowrylander/home/.emacs.d/lib/tag commit --allow-empty-message -am ""
|-git -C ~/shadowrylander/home/.emacs.d/lib/tag push

push-alamode:
|git -C ~/shadowrylander/home/.emacs.d/lib/alamode add .
|-git -C ~/shadowrylander/home/.emacs.d/lib/alamode commit --allow-empty-message -am ""
|-git -C ~/shadowrylander/home/.emacs.d/lib/alamode push

push-all: push push-aiern push-doom-aiern-modeline push-use-package-extras push-tag push-alamode
