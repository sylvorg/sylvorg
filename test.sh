#! /usr/bin/env nix-shell
#! nix-shell -p git-crypt git-filter-repo rsync
#! nix-shell -I nixpkgs=https://github.com/<<username>>/nixpkgs/archive/j.tar.gz
#! nix-shell -i sh
tmpf=$(mktemp)
tmpd=$(mktemp -d)
git encrypted > $tmpf
rsync -avvczz --files-from $tmpf $(git rev-parse --show-toplevel) $tmpd
git -C $(git rev-parse --show-toplevel) filter-repo --paths-from-file $tmpf --invert-paths --force
rsync -avvczz $tmpd/ $(git rev-parse --show-toplevel)/
trap "rm -rf $tmpf $tmpd" 0 2 3 15
