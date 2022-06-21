(import click)
(import json)
(import oreo)
(import os)
(import addict [Dict :as D])
(import bakery [ getconf
                 mkswap
                 mount :as Mount
                 nixos-generate-config
                 nixos-install
                 nixos-rebuild
                 parted
                 rsync
                 sd
                 swapon
                 umount
                 zfs
                 zpool ])
(import functools [partial])
(import pathlib [Path])
(import sys [argv])
(try (import coconut *)
     (except [ImportError] None))
(try (import cytoolz [last])
     (except [ImportError]
             (import toolz [last])))
;; (setv resources (+ (.dirname os.path (.realpath os.path __file__)) "/etc/nixos/"))
(setv resources (+ (.getcwd os) "/etc/nixos/"))
(defn update-datasets [host [swap 0] [encrypted False] [deduplicated False] [pool False] [root-device None] [reserved-only False]]
      (setv snap-dir     [ "snapdir=visible" ]
            extra-copies (+ snap-dir [ "copies=3" ])
            cache        [ "sync=disabled" ]
            ml           "mountpoint=legacy"
            d            "datasets"
            s            "system"
            reserved     "reserved"
            datasets     (D (.loads json (.strip #[[
                                {
                                    "base": {  },
                                    "hold": {  },
                                    "omniverse": {  },
                                    "reserved": {  },
                                    "shadowrylander": { "datasets": {
                                            "oreo": {  },
                                            "sylveon": {  },
                                            "sylvorg": {  },
                                            "syvlorg": {  },
                                            "aiern": {  },
                                            "uru": {  }},
                                        "options": [ "mountpoint=legacy" ]},
                                    "system": {
                                        "datasets": {
                                            "home": { "datasets": { "root": { "mountpoint": "/root" }}},
                                            "nix": {  },
                                            "persist": { "datasets": { "root": { "mountpoint": "/persist/root" }}},
                                            "root": {  },
                                            "tmp": { "datasets": { "nix": {  }}, "options": [ "sync=disabled" ] }
                                        },
                                        "options": [ "mountpoint=legacy" ]
                                    },
                                    "virt": {
                                        "datasets": {
                                            "docker": {  },
                                            "kvm": {  },
                                            "podman": { "datasets": {  }},
                                            "qemu": {  },
                                            "vagrant": {  },
                                            "xen": {  }
                                        },
                                        "options": [ "mountpoint=legacy", "refreservation=none" ],
                                        "mountpoint": "/var/lib"
                                    }
                                }
                         ]])))
            primary-user "shadowrylander"
            users        (D (.loads json (.strip #[[
                                {
                                    "primary": "shadowrylander",
                                    "secondary": "frost",
                                    "nightingale": "curtis"
                                }
                         ]])))
            homes        (D (.loads json (.strip #[[
                                {
                                    "primary": "/home/shadowrylander",
                                    "secondary": "/home/frost",
                                    "nightingale": "/home/curtis"
                                }
                         ]]))))

      #_(assoc datasets host (D { "datasets" { "jails" { "datasets" { "base" (dict)}}}
                                     "options" [ ml ]}))

      (setv (. datasets [host]) (D { "datasets" { "jails" { "datasets" { "base" (dict)}}}
                                     "options" [ ml ]}))

      (for [user (.values users)]

           #_(assoc (. datasets [s] [d] home [d]) user (dict))
           
           (setv (. datasets [s] [d] home [d] [user]) (dict))

           #_(assoc (. datasets [s] [d] persist [d]) user (dict))

           (setv (. datasets [s] [d] persist [d] [user]) (dict))

           #_(assoc (. datasets virt [d] podman [d]) user (dict))

           (setv (. datasets virt [d] podman [d] [user]) (dict))

           )
      (if reserved-only
          (.create zfs (+ host "/" reserved) :o "mountpoint=none")
          (do (with [dnix (open (+ resources "/datasets.nix") "w")]
                    (.write dnix (+ "host: { \n\t\""
                                    (or root-device "${host}/system/root")
                                    "\" = \"/\";"
                                    "\n"))
              (defn recurse [ddict dname droot [mountpoint ""]]
                    (setv recurse/datasets     (.list zfs :r True :o "name" :m/list True :m/ignore-stderr True)
                          recurse/datasets     (cut recurse/datasets 2 (len recurse/datasets))
                          recurse/dataset      (+ droot "/" dname)
                          recurse/real-dataset (.replace recurse/dataset "${host}" host)
                          cloning              (and (!= dname "base")
                                                    (and encrypted deduplicated))
                          prefixes             (, "system"
                                                  "system/root"
                                                  "swap"
                                                  "base"
                                                  "omniverse"
                                                  reserved ))
                    (if cloning
                        (setv clone-or-create  "clone"
                              snapshot-or-none (+ host "/base@root"))
                        (setv clone-or-create  "create"
                              snapshot-or-none ""))
                    (if (not (in recurse/real-dataset (lfor prefix prefixes (+ host "/" prefix))))
                        (do (if (setx recurse/mountpoint (.get ddict "mountpoint" ""))
                                (setv mountpoint recurse/mountpoint)
                                (if mountpoint
                                    (setv mountpoint (+ mountpoint "/" dname)
                                          recurse/mountpoint mountpoint)
                                    (do (setv recurse/mountpoint (.removeprefix recurse/dataset (+ "${host}" "/")))
                                        (for [prefix prefixes]
                                             (setv recurse/mountpoint (.removeprefix recurse/mountpoint (+ prefix "/"))))
                                        (setv recurse/mountpoint (+ "/" recurse/mountpoint)))))
                            (if (and (.startswith recurse/real-dataset (+ host "/" primary-user))
                                     (not (= recurse/real-dataset (+ host "/" primary-user))))
                                (.write dnix (+ "\t\""
                                                recurse/dataset
                                                "\" = [ "
                                                (.join " " (gfor user (.keys users) (+ "\"" (get homes user) "/" dname "\"")))
                                                " ];\n"))
                                #_(for [user (.keys users)]
                                     (.write dnix (+ "\t\""
                                                     recurse/dataset
                                                     "\" = \""
                                                     (+ (get homes user) "/" dname)
                                                     "\";\n")))
                                (.write dnix (+ "\t\""
                                                recurse/dataset
                                                "\" = \""
                                                recurse/mountpoint
                                                "\";\n")))))
                    (if (and pool (not (in recurse/real-dataset recurse/datasets)))
                        (do (zfs :m/subcommand clone-or-create
                                 :o { "repeat-with-values" (.get ddict "options" []) }
                                 snapshot-or-none
                                 recurse/real-dataset)
                            (.snapshot zfs :r True (+ recurse/real-dataset "@blank"))
                            (.hold zfs :r True "blank" (+ recurse/real-dataset "@blank"))))
                    (for [[key value] (.items (.get ddict d (D {  })))]
                         (recurse value key recurse/dataset mountpoint)))
              (for [[key value] (.items datasets)]
                   (recurse value key "${host}"))
              (.write dnix "}"))))
      (if (or pool reserved-only)
          (let [pool-size-plus-metric (get (.get zpool :H True "size" host :m/list True :m/split True) 2)
                pool-size             (round (float (cut pool-size-plus-metric 0 -1)) 2)
                pool-metric           (last pool-size-plus-metric)]
               (defn pool-percentage-value [percentage]
                     #_(-> percentage
                         float
                         (/ 100)
                         (round 2)
                         str
                         (+ pool-metric)
                         return)

                     (return (+ (str (round (/ (float percentage) 100) 2)) pool-metric))

                         )
               (.set zfs
                     (+ "refreservation=" (pool-percentage-value 15))
                     (+ host "/" reserved))

               ;; Apparently, if python internal keywords exist in the argument, such as "set", etc.
               ;; the command errors out; perhaps something to raise an issue of.
               ;; This seems to work as an alternative.
               ;; run(f"zfs set refreservation={pool_percentage_value(15)} {args.Pool}/{reserved}", shell = True)

               (if (not reserved-only)
                   (do (if swap
                           (let [swoptions [ "com.sun:auto-snapshot=false"
                                             "compression=zle"
                                             "logbias=throughput"
                                             "primarycache=metadata"
                                             "secondarycache=none"
                                             "sync=standard" ]
                                 page-size (getconf "PAGESIZE" :m/str True)]
                                (.create zfs
                                         :V (+ (str swap) "G")
                                         :b page-size
                                         :o { "repeat-with-values" swoptions }
                                         (+ host "/swap"))
                                (mkswap (+ "/dev/zvol" host "/swap")))))))))
(setv no-host-error-message "Sorry! The host needs to be set; do this with the main command while running the subcommand!")
#@((.group click :no-args-is-help True)
   (.option click "-d" "--dazzle" :is-flag True)
   (.option click "-H" "--host")
   (.option click "-i" "--inspect" :is-flag True)
   (.option click "-P" "--print-run" :is-flag True :cls oreo.Option :xor [ "print" ])
   (.option click "-p" "--print" :is-flag True :cls oreo.Option :xor [ "print-run" ])
   click.pass-context
   (defn strapper [ ctx dazzle host inspect print-run print ]
         (if (!= (.geteuid os) 0)
             (raise (SystemError "Sorry; this program needs to be run as root!")))
         (.ensure-object ctx dict)
         (setv ctx.obj.host host)
         (if dazzle (.bake-all- getconf :m/dazzle True))
         (if print-run (.bake-all- getconf :m/print-command-and-run True))
         (if print (.bake-all- getconf :m/print-command True))
         (if inspect (.bake-all- getconf :m/debug True))))
#@((.command strapper :no-args-is-help True
                      :context-settings { "ignore_unknown_options" True
                                          "allow_extra_args"       True })
   (.argument click "program-arguments" :nargs -1)
   (.option click "-a" "--all" :is-flag True)
   (.option click "-c" "--copy" :is-flag True)
   (.option click "-g" "--generate" :is-flag True)
   (.option click "-i" "--install" :is-flag True)
   (.option click "-b" "--install-bootloader" :is-flag True :cls oreo.Option :req-one-of [ "install" "all" ])
   (.option click "-r" "--replace" :is-flag True)
   (.option click "-R" "--rebuild")
   click.pass-context
   (defn main [ ctx all copy generate install program-arguments rebuild replace install-bootloader ]
         (if ctx.obj.host
             (do (.bake-all- getconf :m/sudo True)
                 (setv copy-partial (partial rsync :m/run True :a True :v { "repeat" 2 } :c True :z { "repeat" 2 } f"{resources}/"))
                 (if rebuild
                     (do (if copy
                             (copy-partial "/etc/nixos/"))
                         (nixos-rebuild rebuild #* ctx.args :show-trace True))
                     (do (if (or copy all)
                             (do (update-datasets ctx.obj.host)
                                 (copy-partial "/mnt/etc/nixos/")))
                         (if (or generate all)
                             (nixos-generate-config :m/run True :root "/mnt"))
                         (if (or replace all)
                             (if ctx.obj.host
                                 (do (sd :m/run True
                                         "./hardware-configuration.nix"
                                         (+ "./hosts/" ctx.obj.host)
                                         "/mnt/etc/nixos/configuration.nix")
                                     (sd :m/run True
                                         "'device = \"\"'"
                                         "'device = \"!\"'"
                                         "/mnt/etc/nixos/hardware-configuration.nix"))
                                 (raise (NameError no-host-error-message))))
                         (if (or install all)
                             (nixos-install #* ctx.args
                                            :I (with [f (open (+ resources "/flake.lock"))]
                                                     f"nixpkgs=https://github.com/nixos/nixpkgs/archive/{(. (D (.load json f)) nodes nixos-22-05 locked rev)}.tar.gz")
                                            :m/run True
                                            :show-trace True
                                            :install-bootloader install-bootloader
                                            :option "tarball-ttl 0"
                             )))))
             (raise (NameError no-host-error-message)))))
#@((.command strapper :no-args-is-help True)
   (.option click "-B" "--boot-device" :type (, str int))
   (.option click "-c" "--copies" :type int :default 1)
   (.option click "-d" "--deduplicated" :is-flag True)
   (.option click "-e" "--encrypted" :is-flag True)
   (.option click "-M" "--host-mountpoint" :help "Use the hostname as the mountpoint" :is-flag True :cls oreo.Option :xor [ "mountpoint" ])
   (.option click "-m" "--mountpoint" :cls oreo.Option :xor [ "host-mountpoint" ])
   (.option click "-o" "--pool-options" :multiple True)
   (.option click "-O" "--dataset-options" :multiple True)
   (.option click
            "-P"
            "--partition"
            :multiple True
            :cls oreo.Option
            :xor [ "raid" ]
            :help "Set up an entire disk; a single `-P' sets up the boot partition with the size as the value passed in (with the unit, such as `2G' for 2 gibibytes),
a second `-P' sets up the swap space similarly, and subsequent invocations sets up further unformatted partitions.
The final partition will be the ZFS partition, and does not need to be specified.")
   (.option click "-p" "--pool-only" :is-flag True)
   (.option click "-r" "--raid" :cls oreo.Option :xor [ "partition" ])
   (.option click "-S" "--swap-device" :type (, str int))
   (.option click "-s" "--swap" :type int :default 0)
   (.option click "-z" "--zfs-devices" :required True :multiple True)
   click.pass-context
   (defn create [ ctx boot-device copies deduplicated encrypted host-mountpoint mountpoint dataset-options pool-options partition pool-only raid swap-device swap zfs-devices ]
         (if ctx.obj.host
             (try (if (= (input "THIS WILL DELETE ALL DATA ON THE SELECTED DEVICE / PARTITION! TO CONTINUE, TYPE IN 'ZFS CREATE'!\n\t") "ZFS CREATE")
                      (let [dataset-options-dict (D { "xattr"      "sa"
                                                      "acltype"    "posixacl"
                                                      "mountpoint"  (if host-mountpoint
                                                                        (+ "/" ctx.obj.host)
                                                                        (or mountpoint "none"))
                                                      "compression" "zstd-19"
                                                      "checksum"    "edonr"
                                                      "atime"       "off"
                                                      "relatime"    "off"
                                                      "copies"      copies })
                            pool-options-dict (D { "autotrim" "on"
                                                   "altroot" "/mnt"
                                                   "autoexpand" "on" })
                            command (partial zpool.create :f True :m/run True)
                            no-raid-error-message "Sorry! For multiple zfs devices a raid configuration must be provided using `-r / --raid'!"
                            zfs-device (if (= (len zfs-devices) 1)
                                        (if raid
                                            (raise (NameError no-raid-error-message))
                                            (get zfs-devices 0))
                                        (if raid
                                            #[f[{raid} {(.join " " zfs-devices)}]f]
                                            (raise (NameError no-raid-error-message))))]
                           (if (or partition boot-device)
                               (.bake- parted :m/sudo True :s True :a "optimal" "--"))
                           (if partition
                               (do (setv zfs-name ctx.obj.host)
                                   (parted zfs-device "mklabel" "gpt")
                                   (for [[i p] (enumerate partition)]
                                        (parted zfs-device
                                                "mkpart"
                                                "primary"

                                                #_(if i (get partition (dec i)) "0%")

                                                (if i (get partition (- i 1)) "0%")

                                                p))
                                   (parted zfs-device "mkpart" "primary" (get partition -1) "100%")
                                   (parted zfs-device "name" (if (> (len partition) 1) 3 2) zfs-name)))
                           (if (or partition boot-device)
                               (if boot-device
                                   (let [ device (get boot-device 0)
                                          index  (get boot-device 1) ]
                                        (parted device "mkfs" index "fat32")
                                        (parted device "set" index "boot" "on")
                                        (parted device "set" index "esp" "on"))
                                   (do (parted zfs-device "name" 1 (+ ctx.obj.host "-boot"))
                                       (parted zfs-device "mkfs" 1 "fat32")
                                       (parted zfs-device "set" 1 "boot" "on")
                                       (parted zfs-device "set" 1 "esp" "on"))))
                           (if (or (> (len partition) 1) swap-device)
                               (if swap-device
                                   (parted (get swap-device 0) "mkfs" (get swap-device 1) "linux-swap")
                                   (do (parted zfs-device "name" 2 (+ ctx.obj.host "-swap"))
                                       (parted zfs-device "mkfs" 2 "linux-swap"))))
                           (for [dataset (.list zfs :r True :H True :m/list True :m/split True)]
                                (if (in ctx.obj.host dataset)
                                    (.export zpool :f True ctx.obj.host :m/ignore-stderr True)))
                           (if encrypted
                               (setv dataset-options-dict.encryption "aes-256-gcm"
                                     dataset-options-dict.keyformat  "passphrase"))
                           (if deduplicated
                               (setv dataset-options-dict.dedup "edonr,verify"))
                           (if (.ismount os.path "/mnt")
                               (umount :R True "/mnt"))
                           (.export zpool :f True ctx.obj.host :m/ignore-stderr True)
                           (.update dataset-options-dict (dfor item pool-options :setv kv (.split item "=") [(get kv 0) (get kv 1)]))
                           (.update pool-options-dict (dfor item dataset-options :setv kv (.split item "=") [(get kv 0) (get kv 1)]))
                           (command :O { "repeat-with-values" (gfor [k v] (.items dataset-options-dict) f"{k}={v}") }
                                    :o { "repeat-with-values" (gfor [k v] (.items pool-options-dict) f"{k}={v}") }
                                    ctx.obj.host
                                    (if partition (+ "/dev/disk/by-label/" zfs-name) zfs-device))
                           (update-datasets ctx.obj.host swap encrypted deduplicated :pool True :reserved-only pool-only))
                      (print "Sorry; not continuing!\n\n"))
                  (finally (.export zpool :f True ctx.obj.host :m/ignore-stderr True)))
             (raise (NameError no-host-error-message)))))
#@((.command strapper :no-args-is-help True)
   (.option click "-b" "--boot-device")
   (.option click "-d" "--deduplicated" :is-flag True)
   (.option click "-e" "--encrypted" :is-flag True)
   (.option click "-r" "--root-device")
   (.option click "-s" "--swap" :cls oreo.Option :xor [ "swap-device" ] :is-flag True)
   (.option click "-S" "--swap-device" :cls oreo.Option :xor [ "swap" ])
   (.option click "-i" "--install" :is-flag True)
   (.option click "-I" "--install-bootloader" :is-flag True)
   click.pass-context
   (defn mount [ ctx boot-device deduplicated encrypted root-device swap swap-device install install-bootloader ]
         (if ctx.obj.host
             (do (update-datasets ctx.obj.host :root-device root-device :encrypted encrypted :deduplicated deduplicated :swap swap)
                 (for [dataset (.list zfs :r True :H True :m/list True :m/split True)]
                      (if (in ctx.obj.host dataset)
                          (break))
                      (else (.import zpool :f True ctx.obj.host)))
                 (if encrypted
                     (.load-key zfs ctx.obj.host))
                 (try (.mkdir (Path "/mnt"))
                      (except [FileExistsError]
                              (if (.ismount os.path "/mnt")
                                  (umount :R True "/mnt"))))
                 (if root-device
                     (Mount root-device "/mnt")
                     (Mount :t "zfs" (+ ctx.obj.host "/system/root") "/mnt"))
                 (try (.mkdir (Path "/mnt/mnt"))
                      (except [FileExistsError]
                              (if (.ismount os.path "/mnt/mnt")
                                  (umount :R True "/mnt/mnt"))))
                 (Mount :bind True "/mnt" "/mnt/mnt")
                 (.mkdir (Path "/mnt/etc/nixos") :parents True :exist-ok True)

                 (.mkdir (Path "/mnt/nix") :parents True :exist-ok True)
                 (Mount :t "zfs" (+ ctx.obj.host "/system/nix") "/mnt/nix")

                 (.mkdir (Path "/mnt/persist") :parents True :exist-ok True)
                 (Mount :t "zfs" (+ ctx.obj.host "/system/persist") "/mnt/persist")

                 (if boot-device
                     (let [boot "/mnt/boot/efi"]
                          (.mkdir (Path boot) :parents True :exist-ok True)
                          (Mount boot-device boot)))
                 (if swap
                     (swapon (+ "/dev/zvol/" ctx.obj.host "/swap" :m/run True)))
                 (if swap-device
                     (swapon swap-device :m/run True))

                 (.mkdir (Path "/tmp") :parents True :exist-ok True)
                 (Mount :t "zfs" (+ ctx.obj.host "/system/tmp") "/tmp" :m/run True)

                 (.mkdir (Path "/tmp/nix") :parents True :exist-ok True)
                 (Mount :t "zfs" (+ ctx.obj.host "/system/tmp/nix") "/tmp/nix" :m/run True)

                 ;; (rsync :a True :v { "repeat" 2 } :c True :z { "repeat" 2 } :delete True "/nix/" "/tmp/nix/")
                 ;; (Mount :t "zfs" (+ ctx.obj.host "/system/tmp/nix") "/nix" :m/run True)

                 (if (or install install-bootloader)
                     (.invoke ctx main :all True :install-bootloader install-bootloader)))
             (raise (NameError no-host-error-message)))))
#@((.command strapper)
   (.option click "-d" "--deduplicated" :is-flag True)
   (.option click "-e" "--encrypted" :is-flag True)
   (.option click "-f" "--files" :is-flag True :help "Update datasets.nix with any new datasets; the default")
   (.option click "-p" "--pool" :is-flag True :help "Update the pool and datasets.nix with any new datasets")
   (.option click "-r" "--root-device")
   (.option click "-s" "--swap" :type int :default 0)
   click.pass-context
   (defn update [ ctx deduplicated encrypted files pool root-device swap ]
         (if ctx.obj.host
             (try (setv ud (partial update-datasets ctx.obj.host :swap swap :encrypted encrypted :deduplicated deduplicated :root-device root-device))
                  (cond [files (ud)]
                        [pool (ud :pool True)]
                        [True (ud)])
                  (finally (.export zpool :f True ctx.obj.host :m/ignore-stderr True)))
             (raise (NameError no-host-error-message)))))
