#!/usr/bin/env sh
nix-channel --add https://github.com/nixos/nixpkgs/archive/master/nixpkgs.tar.gz master
nix-channel --update
nix-env -iA master.emacs master.python310
git -C ~/shadowrylander/settings checkout main
~/shadowrylander/settings/org-tangle.sh oreo.aiern.org README.org
chmod +x ~/shadowrylander/wheee.py ~/shadowrylander/bootstrap.py
git clone https://github.com/shadowrylander/shadowrylander ~/shadowrylander/shadowrylander
