#! /usr/bin/env nix-shell
#[9debb8f7-e891-48e9-8bd9-3b5b40d0ebbb[
#! nix-shell -p python310 python310Packages.bakery
#! nix-shell -I nixpkgs=https://github.com/shadowrylander/nixpkgs/archive/j.tar.gz
#! nix-shell -i hy
]9debb8f7-e891-48e9-8bd9-3b5b40d0ebbb]
(import bakery [zfs])
(import sys [argv exit])
(for [pool (cut argv 1 None)]
     (for [dataset (.list zfs :H True :r True :o "mountpoint,mounted" pool)]
          (let [ dataset-split (.split dataset) ]
               (if (and (!= (get dataset-split 0) "none") (= (get dataset-split 1) "no"))
                   (exit 75)))))
