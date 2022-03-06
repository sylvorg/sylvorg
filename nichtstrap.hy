#!/usr/bin/env hy
(import click)
(import json)
(import os)
(import addict [Dict :as D])
(import bakery [ getconf
                 mkswap
                 mount :as Mount
                 nixos-generate-config
                 nixos-install
                 nixos-rebuild
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
(require hyrule [-> assoc])
(setv resources (+ (.dirname os.path (.realpath os.path __file__)) "/etc/nixos/"))
(defn update-datasets [host [swap 0] [encrypted False] [deduplicated False] [pool False] [root-device None]]
      (setv snap-dir     [ "snapdir=visible" ]
            extra-copies (+ snap-dir [ "copies=3" ])
            cache        [ "sync=disabled" ]
            ml           "mountpoint=legacy"
            d            "datasets"
            s            "system"
            datasets     (D (.loads json (.strip #[[
                                {
                                    "base": {  },
                                    "omniverse": {  },
                                    "reserved": {  },
                                    "shadowrylander": { "datasets": {
                                            "oreo": { "mountpoint": "/home/shadowrylander/oreo", "options": [ "snapdir=visible" ] },
                                            "sylveon": { "mountpoint": "/home/shadowrylander/sylveon", "options": [ "snapdir=visible" ] },
                                            "sylvorg": { "mountpoint": "/home/shadowrylander/sylvorg", "options": [ "snapdir=visible" ] },
                                            "syvlorg": { "mountpoint": "/home/shadowrylander/syvlorg", "options": [ "snapdir=visible" ] },
                                            "dross": { "mountpoint": "/home/shadowrylander/dross", "options": [ "snapdir=visible" ] },
                                            "uru": { "mountpoint": "/home/shadowrylander/uru", "options": [ "snapdir=visible" ] }},
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
            users        (D (.loads json (.strip #[[
                                {
                                    "primary": "shadowrylander",
                                    "secondary": "frost",
                                    "nightingale": "curtis"
                                }
                         ]]))))
      (assoc datasets "${host}" (D { "datasets" { "jails" { "datasets" { "base" (dict)}}}
                                     "options" [ ml ]}))
      (for [user (.values users)]
           (assoc (. datasets [s] [d] home [d]) user (dict))
           (assoc (. datasets virt [d] podman [d]) user (dict)))
      (with [dnix (open (+ resources "/datasets.nix") "w")]
            (.write dnix (+ "host: { \""
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
                                                "hold"
                                                "omniverse"
                                                "reserved" ))
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
                                  (do (setv recurse/mountpoint (.removeprefix recurse/dataset "${host}/"))
                                      (for [prefix prefixes]
                                           (setv recurse/mountpoint (.removeprefix recurse/mountpoint (+ prefix "/"))))
                                      (setv recurse/mountpoint (+ "/" recurse/mountpoint)))))
                          (.write dnix (+ "\t\""
                                          recurse/dataset
                                          "\" = \""
                                          recurse/mountpoint
                                          "\";\n"))))
                  (if (and pool (not (in recurse/real-dataset recurse/datasets)))
                      (do (zfs :m/subcommand clone-or-create
                               :o { "repeat-with-values" (.get ddict "options" []) }
                               snapshot-or-none
                               recurse/real-dataset)
                          (.snapshot zfs :r True (+ recurse/real-dataset "@root"))))
                  (for [[key value] (.items (.get ddict d (D {  })))]
                       (recurse value key recurse/dataset mountpoint)))
            (for [[key value] (.items datasets)]
                 (recurse value key "${host}"))
            (.write dnix "}"))
      (if pool
          (let [pool-size-plus-metric (get (.get zpool :H True "size" host :m/list True :m/split True) 2)
                pool-size             (-> pool-size-plus-metric
                                          (cut 0 -1)
                                          (float)
                                          (round 2))
                pool-metric           (last pool-size-plus-metric)]
               (defn pool-percentage-value [percentage]
                     (-> percentage
                         (float)
                         (/ 100)
                         (round 2)
                         (str)
                         (+ pool-metric)
                         (return)))
               (.set zfs
                     (+ "refreservation=" (pool-percentage-value 15))
                     (+ host "/reserved"))

               ;; Apparently, if python internal keywords exist in the argument, such as "set", etc.
               ;; the command errors out; perhaps something to raise an issue of.
               ;; This seems to work as an alternative.
               ;; run(f"zfs set refreservation={pool_percentage_value(15)} {args.Pool}/reserved", shell = True)

               (if swap
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
                        (mkswap (+ "/dev/zvol" host "/swap")))))))
(setv no-host-error-message "Sorry! The host needs to be set; do this with the main command while running the subcommand!")
#@((.group click :no-args-is-help True)
   (.option click "-d" "--dazzle" :is-flag True)
   (.option click "-H" "--host")
   (.option click "-i" "--inspect" :is-flag True)
   (.option click "-p" "--print-run" :is-flag True)
   click.pass-context
   (defn nichtstrap [ ctx dazzle host inspect print-run ]
         (.ensure-object ctx dict)
         (setv ctx.obj.host host)
         (if print-run (.bake-all- getconf :m/print-command-and-run True))
         (if inspect (.bake-all- getconf :m/debug True))))
#@((.command nichtstrap :no-args-is-help True
                        :context-settings { "ignore_unknown_options" True
                                            "allow_extra_args"       True })
   (.argument click "program-arguments" :nargs -1)
   (.option click "-a" "--all" :is-flag True)
   (.option click "-c" "--copy" :is-flag True)
   (.option click "-g" "--generate" :is-flag True)
   (.option click "-i" "--install" :is-flag True)
   (.option click "-r" "--replace" :is-flag True)
(.option click "-R" "--rebuild")
click.pass-context
(defn main [ ctx all copy generate install program-arguments rebuild replace ]
      (.bake-all- getconf :m/sudo True)
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
                              (+ "./configs/" ctx.obj.host)
                              "/mnt/etc/nixos/configuration.nix")
                          (sd :m/run True
                              "device = \"\""
                              "device = \"!\""
                              "/mnt/etc/nixos/hardware-configuration.nix"))
                      (raise (NameError no-host-error-message))))
              (if (or install all)
                  (nixos-install #* ctx.args
                                 :I "nixpkgs=https://github.com/shadowrylander/nixpkgs/archive/j.tar.gz"
                                 :m/run True
                                 :show-trace True
:option "tarball-ttl 0"
))))))
#@((.command nichtstrap :no-args-is-help True)
   (.option click "-d" "--deduplicated" :is-flag True)
   (.option click "-e" "--encrypted" :is-flag True)
   (.option click "-s" "--swap" :type int :default 0)
   (.option click "-z" "--zfs-device" :required True)
   click.pass-context
   (defn create [ ctx deduplicated encrypted swap zfs-device ]
         (if ctx.obj.host
             (try (if (= (input "THIS WILL DELETE ALL DATA ON THE SELECTED DEVICE / PARTITION! TO CONTINUE, TYPE IN 'ZFS CREATE'!\n\t") "ZFS CREATE")
                      (let [options (D { "xattr"      "sa"
                                         "acltype"    "posixacl"
                                         "mountpoint"  "none"
                                         "compression" "zstd-19"
                                         "checksum"    "edonr"
                                         "atime"       "off"
                                         "relatime"    "off" })
                            command (partial zpool.create
                                             :f True
                                             :o { "repeat-with-values" (, "autotrim=on" "altroot=/mnt" "autoexpand=on") })]
                           (for [dataset (.list zfs :r True :H True :m/list True :m/split True)]
                                (if (in ctx.obj.host dataset)
                                    (.export zpool :f True ctx.obj.host :m/ignore-stderr True)))
                           (if encrypted
                               (setv options.encryption "aes-256-gcm"
                                     options.keyformat  "passphrase"))
                           (if deduplicated
                               (setv options.dedup "edonr,verify"))
                           (if (.ismount os.path "/mnt")
                               (umount :R True "/mnt"))
                           (.export zpool :f True ctx.obj.host :m/ignore-stderr True)
                           (command :O { "repeat-with-values" (gfor [k v] (.items options) f"{k}={v}") } ctx.obj.host zfs-device)
                           (update-datasets ctx.obj.host swap encrypted deduplicated :pool True))
                      (print "Sorry; not continuing!\n\n"))
                  (finally (.export zpool :f True ctx.obj.host :m/ignore-stderr True)))
             (raise (NameError no-host-error-message)))))
#@((.command nichtstrap :no-args-is-help True)
   (.option click "-b" "--boot-device")
   (.option click "-d" "--deduplicated" :is-flag True)
   (.option click "-e" "--encrypted" :is-flag True)
   (.option click "-r" "--root-device")
   (.option click "-s" "--swap" :is-flag True)
   click.pass-context
   (defn mount [ ctx boot-device deduplicated encrypted root-device swap ]
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
    (Mount :t "zfs" (+ ctx.obj.host "/system/nix") "/mnt/nix")
    (Mount :t "zfs" (+ ctx.obj.host "/system/persist") "/mnt/persist")

    (setv etc/nixos "/etc/nixos"
          men (+ "/mnt" etc/nixos)
          mpn (+ "/mnt/persist/root" etc/nixos))
    (.mkdir (Path men) :parents True :exist-ok True)
    (.mkdir (Path mpn) :parents True :exist-ok True)
    (Mount :bind True mpn men)

    (if boot-device
        (let [boot "/mnt/boot/efi"]
             (.mkdir (Path boot) :parents True :exist-ok True)
             (Mount boot-device boot)))
    (if swap
        (swapon (+ "/dev/zvol/" ctx.obj.host "/swap")))

    ;; (Mount :t "zfs" (+ ctx.obj.host "/system/tmp") "/tmp")
    ;; (Mount :t "zfs" (+ ctx.obj.host "/system/tmp/nix") "/tmp/nix")
    ;; (rsync :a True :v { "repeat" 2 } :c True :z { "repeat" 2 } :delete True "/nix/" "/tmp/nix/")
    ;; (Mount :t "zfs" (+ ctx.obj.host "/system/tmp/nix") "/nix")

    )
(raise (NameError no-host-error-message)))))
#@((.command nichtstrap :no-args-is-help True)
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
