#! /usr/bin/env nix-shell
#[9debb8f7-e891-48e9-8bd9-3b5b40d0ebbb[
#! nix-shell borgmatic-shell.nix
#! nix-shell -i hy
]9debb8f7-e891-48e9-8bd9-3b5b40d0ebbb]
(import bakery [zfs])
(import sys [argv exit])
(for [pool (cut argv 1 None)]
     (for [dataset (.list zfs :H True :r True :o "mountpoint,mounted" pool)]
          (let [ dataset-split (.split dataset) ]
               (if (and (!= (get dataset-split 0) "none") (= (get dataset-split 1) "no"))
                   (exit 75)))))
