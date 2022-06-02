with builtins; args@{ config, ... }: let
    flake = import ./.;
    system = args.system or currentSystem;
    host = args.host or config.networking.hostName;
    fromFlake = args ? inputs;
    inheritanceSet = if fromFlake then args else (flake.make.specialArgs host system);
    inherit (inheritanceSet) lib overlays nixpkgset pkgs;
    dir = "/home/shadowrylander/aiern";
    dirExists = pathExists dir;
    repo = with lib; j.mntConvert (if dirExists then (fetchGit { url = "file://${dir}"; ref = "main"; }) else flake.inputs.${j.attrs.users.primary});
    nixos = "${(args.nixpkgs or <nixpkgs>)}/nixos";
    nixos-configuration = configuration: import nixos { configuration = import configuration (lib.recursiveUpdate args inheritanceSet); inherit system; };
    nixos-configurations = {
        server = nixos-configuration ./profiles/server.nix;
        configuration = nixos-configuration ./configuration.nix;
        hardware-configuration = import nixos { inherit system; configuration.imports = [
            ./hardware-configuration.nix
            ({config, ... }: { networking.hostId = substring 0 8 (readFile "/etc/machine-id"); boot.loader.grub.devices = [ "nodev" ]; })
        ]; };
    };
in with lib; {
    imports = with flake.inputs; flatten [
        ./cachix.nix
        (if fromFlake then [] else [ home-manager.nixosModules.home-manager impermanence.nixosModules.impermanence ])
        ./variables.nix
    ];
    config = mkMerge [
        # (removeAttrs nixos-configurations.hardware-configuration.config [ "fileSystems" "nesting" "jobs" "fonts" "meta" "documentation" ])
        (filterAttrs (n: v: elem n [ "boot" "powerManagement" "hardware" ]) nixos-configurations.hardware-configuration.config)

        # TODO: What exactly from `system' am I taking? Merge it explicitly.
        # (if fromFlake then (filterAttrs (n: v: elem n [ "system" ]) nixos-configurations.configuration.config) else {})
( mkIf config.variables.zfs {
    boot = {
        extraModulePackages = with config.boot.kernelPackages; [ zfsUnstable ];
        kernelModules = [ "zfs" ];
        kernelParams = [ "nohibernate" ];
        loader.grub.zfsSupport = true;
        initrd = {
            postDeviceCommands = mkAfter (concatMapStrings (d: "zfs rollback -r ${d}@blank\n") (filter (d: (j.has.prefix [
                "${host}/system/home"
            ] d) || (elem d [
                "${host}/system/root"
                # "${host}/system/tmp"
            ])) (attrNames j.attrs.datasets.fileSystems)));
            kernelModules = [ "zfs" "r8169" ];
            availableKernelModules = config.boot.initrd.kernelModules;
        };
        zfs = {
            requestEncryptionCredentials = true;
            enableUnstable = true;
            devNodes = "/dev/";
        };
    };
    environment = {
        persistence = let
            rootDirSet = {
                user = "root";
                group = "root";
            };
            rootFileSet.parentDirectory = rootDirSet;
        in {
            "/persist/root" = let
                etc-prefixes = [ "nixos" "containers" "NetworkManager/system-connections" "tailscale" ];
            in {
                hideMounts = true;
                files = unique (map (file: if (isString file) then (recursiveUpdate { inherit file; } rootFileSet) else (recursiveUpdate rootFileSet file)) (flatten [
                    "/etc/host"
                    "/etc/machine-id"
                    (map (directory: j.recurseDir { dir = "${repo}/${directory}"; local = 0; ignores.prefix = (map (d: d + "/") etc-prefixes); }) [
                        "etc"
                        "var"
                    ])
                ]));
                directories = unique (map (directory: if (isString directory) then (recursiveUpdate { inherit directory; } rootDirSet) else (recursiveUpdate rootDirSet directory)) (flatten [
                    (map (d: "/etc/" + d) etc-prefixes)

                    "/bin"

                    # TODO: Prevents `sshd_config' itself from being created
                    # "/etc/ssh"

                    "/sbin"
                    "/snap"
                    "/usr"
                    "/var/lib/acme"
                    "/var/lib/bluetooth"
                    "/var/lib/systemd/coredump"
                    "/var/log"

                    # Managed by the `var' module
                    # "/var/lib/tailscale"

                    config.services.tailscale.authenticationConfirmationFile
                ]));
            };
            "/persist" = let
                redRepo = readDir repo;
                redRepoFiles = flatten [
                    (attrNames (filterAttrs (n: v: v != "directory") redRepo))
                ];
                redRepoDirectories = flatten [
                    (attrNames (filterAttrs (n: v: v == "directory") redRepo))
                ];
            in {
                users = listToAttrs (map (user: let
                    home = j.attrs.allHomes.${user};
                    userDirSet = {
                        inherit user;
                        group = user;
                    };
                    userFileSet.parentDirectory = userDirSet;
                    predRepo = let pHome = "/persist/${home}"; in j.readDirExists pHome;
                    predRepoFiles = flatten [
                        (attrNames (filterAttrs (n: v: v != "directory") predRepo))
                    ];
                    predRepoDirectories = flatten [
                        (attrNames (filterAttrs (n: v: v == "directory") predRepo))
                    ];
                in nameValuePair user {
                    inherit home;
                    files = unique (map (file: if (isString file) then (recursiveUpdate { inherit file; } userFileSet) else (recursiveUpdate userFileSet file)) (flatten [
                        ".bash_history"
                        ".emacs-profile"
                        ".fasd"
                        ".gitignore"
                        ".globalignore"
                        ".nix-channels"
                        ".python-history"
                        ".screenrc"
                        ".viminfo"
                        ".zsh_history"
                        config.services.vaddy.dataDir
                        redRepoFiles
                    ]));
                    directories = unique (map (directory: if (isString directory) then (recursiveUpdate { inherit directory; } userDirSet) else (recursiveUpdate userDirSet directory)) (flatten [
                        ".atom"
                        ".byobu"
                        ".cache"
                        ".config"
                        ".linuxbrew"
                        ".local"
                        ".mozilla"
                        ".peru"
                        ".pki"
                        ".repos"
                        ".user"
                        ".vim_runtime"
                        ".virtualenvs"
                        ".vscode-oss"
                        ".vscode"
                        ".yubico"
                        ".z"
                        "Documents"
                        "Downloads"
                        "keybase"
                        "Music"
                        "nix-plugins"
                        "Pictures"
                        "Public"
                        "Templates"
                        "tests"
                        "Videos"
                        "VirtualBox VMs"
                        { directory = ".gnupg"; mode = "0700"; }
                        { directory = ".nixops"; mode = "0700"; }
                        { directory = ".ssh"; mode = "0700"; }
                        { directory = ".gnupgk"; mode = "0700"; }
                        redRepoDirectories
                    ]));}) j.attrs.allUsers);
            };
        };
    };
    fileSystems = (mapAttrs' (dataset: mountpoint: nameValuePair mountpoint (
        mkForce (recursiveUpdate base { device = dataset; ${
            j.mif.null ((j.has.infix [
                j.attrs.users.primary
                "persist"
                "home"
            ] dataset) || (elem dataset [ ])) "neededForBoot"
        } = true; })
    )) j.attrs.datasets.fileSystems);
    services = {
        sanoid = let
            sanoidBase = {
                useTemplate = [ "base" ];
                recursive = true;
            };
            disabled = { processChildrenOnly = true; };
        in {
            enable = true;
            templates."base" = {
                autoprune = true;
                autosnap = true;
                interval = "30s";

                # 6 snapshots an hour
                daily = 144;

                # 2 snapshots a minute
                hourly = 120;

                # 6 snapshots a day for 28 days
                monthly = 168;

                # Twice the weeks in a year
                yearly = 104;
            };

            datasets = listToAttrs (map (dataset: nameValuePair "${host}/${dataset}" sanoidBase) (flatten [
                j.attrs.datasets.backup
                host
            ]));
        };
        syncoid = let
            syncoidBase = mkMerge [{
                recursive = true;
                commonArgs = [
                    "--compress zstd-slow"
                    "--no-stream"
                    "--no-sync-snap"
                    "--create-bookmark"
                ];
                }

                # TODO
                # (mkIf vars.encrypted {
                #     sendOptions = "vvwRI";
                #     recvOptions = "vvFs";
                # })
                # (mkIf (!vars.encrypted) {
                #     recvOptions = "vvFds";
                #     sendOptions = "vvRI";
                # })

            ];
        in {
            enable = false;
            sshKey = "/root/.ssh/id_ed25519";
            commands = listToAttrs (map (dataset: nameValuePair "${host}/${dataset}" (recursiveUpdate syncoidBase { target = ""; })) (flatten [
                j.attrs.datasets.backup
                host
            ]));
        };
        udev.extraRules = ''
            ACTION=="add|change", KERNEL=="sd[a-z]*[0-9]*|mmcblk[0-9]*p[0-9]*|nvme[0-9]*n[0-9]*p[0-9]*", ENV{ID_FS_TYPE}=="zfs_member", ATTR{../queue/scheduler}="none"
        ''; # zfs already has its own scheduler. without this my(@Artturin) computer froze for a second when i nix build something.
        zfs = {
            trim.enable = true;
            autoScrub.enable = true;

            # Managed by Sanoid
            autoSnapshot.enable = false;
        };
    };
    virtualisation = {
        containers.storage.settings.storage.driver = "zfs";
        lxd.zfsSupport = true;
        podman.extraPackages = [ pkgs.zfs ];
        docker.storageDriver = "zfs";
    };
})
{
    boot = {
        supportedFilesystems = j.attrs.fileSystems.supported;
        initrd = {
            inherit (config.boot) supportedFilesystems;
            compressor = "${lib.getBin pkgs.zstd}/bin/zstd";
            network.ssh.enable = true;
            extraModprobeConfig = '' options kvm_intel_nested=1 '';
        };
        loader = {
            generic-extlinux-compatible.enable = mkForce false;
            systemd-boot = {
                configurationLimit = 25;
                editor = mkForce false;
                # enable = mkForce false;
                enable = mkForce true;
            };
            grub = {
                # enable = mkForce true;
                enable = mkForce false;
                efiSupport = true;
                efiInstallAsRemovable = mkForce false;
                # efiInstallAsRemovable = mkForce true;
                # devices = [ "nodev" ];
                # device = "nodev";
                device = if config.boot.loader.grub.efiSupport then config.boot.loader.efi.efiSysMountPoint else "/boot";
                version = 2;
                useOSProber = true;

                # TODO: Get more options
                extraEntries = ''
                    menuentry "Reboot" { reboot }
                    menuentry "Poweroff" { halt }
                '';
            };
            efi = {
                canTouchEfiVariables = mkForce true;
                # canTouchEfiVariables = mkForce false;
                efiSysMountPoint = "/boot/efi";
            };
            timeout = 10;

            # Used for Bedrock Linux
            # Also causing EFI stuff not to be installed
            # initScript.enable = mkForce true;

        };
        kernelPackages = mkDefault pkgs.linuxPackages_xanmod;
        # kernelPackages = mkDefault pkgs.linuxPackages_lqx;
        # kernelPackages = mkDefault pkgs.linuxPackages_zen;
        kernelPatches = flatten [
            (optionals (elem "bcachefs" config.boot.supportedFilesystems) (filter (set: hasInfix "bcachefs" set.name) pkgs.linuxKernel.kernels.linux_testing_bcachefs.kernelPatches))
        ];
    };
}
{
    console = {
        # Select internationalisation properties.
        # font = lib.mkDefault "${terminus_font}/share/consolefonts/ter-u28n.psf.gz";
        font = "Cartograph CF Extra Bold Italic";
        keyMap = "us";
    };
}
{
    environment = {
        etc."nix/nix.conf".text = mkForce j.attrs.configs.nix;
        systemPackages = with pkgs; flatten [
            (pass.withExtensions (ext: with ext; [pass-tomb pass-genphrase]))
            assh
            cachix
            direnv
            exa
            fasd
            fd
            fzf
            git-crypt
            gnupg
            gopass
            hub
            mosh
            sqlite
            starship
            tailscale
            tailapi
            zoxide
        ];
    };
}
{
    zramSwap = {
        enable = true;
        algorithm = "zstd";
    };
}
{
    fileSystems = let
        inherit (j.attrs.fileSystems) base;
    # in filterAttrs (n: v: !elem "bind" v.options) nixos-configurations.hardware-configuration.config.fileSystems;
    in filterAttrs (n: v: elem n [ "/boot" "/boot/efi" ]) nixos-configurations.hardware-configuration.config.fileSystems;
}
{
    hardware = {
        enableRedistributableFirmware = lib.mkDefault true;
        # Enable sound
        pulseaudio.enable = true;
    };
    sound.enable = true;
}
{
    networking = {
        networkmanager.enable = mkForce true;
        interfaces = if fromFlake then (mapAttrs (n: v: recursiveUpdate v {
            useDHCP = mkForce (! config.networking.networkmanager.enable);
            wakeOnLan.enable = true;
        }) (filterAttrs (n: v: !elem n [ "wg0" ]) nixos-configurations.configuration.config.networking.interfaces)) else {};

        # The global useDHCP flag is deprecated, therefore explicitly set to false here.
        # Per-interface useDHCP will be mandatory in the future, so this generated config
        # replicates the default behaviour.
        useDHCP = false;

        # Configure network proxy if necessary
        # proxy = {
        # default = "http://user:password@proxy:port/";
        # noProxy = "127.0.0.1,localhost,internal.domain";
        # };

        wireguard.interfaces.wg0 = {
            generatePrivateKeyFile = true;
            privateKeyFile = "/persist/etc/wireguard/wg0";
        };

        firewall = recursiveUpdate {
            enable = true;
        } (if config.variables.relay then {
            allowedTCPPorts = [ 80 222 443 2022 8080 9418 ];
        } else if config.variables.server then {
            allowedTCPPorts = [ ];
        } else {
            allowedTCPPorts = [ ];
        });
    };
}
{
    nix = rec {
        gc = j.foldToSet [
            { automatic = true; }
            { dates = "monthly"; }
            # {
            #   dates = "monthly";
            #   options = "-d";
            # }
            # {
            #   dates = "daily";
            #   options = "--delete-older-than 30d";
            # }
        ];
        optimise = {
            automatic = true;
            dates = [ "05:00" ];
        };
        extraOptions = j.attrs.configs.nix;
        settings = {
            auto-optimise-store = true;
            sandbox = true;
        };
        # sandboxPaths = [];
    };
}
{ nixpkgs = nixpkgset; }
{
    services.logind.lidSwitch = "hybrid-sleep";
    powerManagement = {
        enable = true;
        cpuFreqGovernor = mkForce "ondemand";
    };
}
{
    programs = {
        xonsh.enable = true;
        fish.enable = true;
        zsh.enable = true;
        tmux.enable = true;
        fuse = {
            enable = true;
            userAllowOther = true;
        };
        mosh = {
            enable = true;
            openFirewall = true;
        };
    };
}
{
    services = {
        tailscale.enable = true;
        openssh = mkForce nixos-configurations.server.config.services.openssh;
        xserver = {
            enable = true;
            layout = "us";
            # xkbOptions = "eurosign:e";
            # Enable touchpad support.
            libinput = {
                enable = true;
                touchpad = {
                    naturalScrolling = true;
                    middleEmulation = true;
                    tapping = true;
                };
            };
            # synaptics.enable = true;
            desktopManager.gnome.enable = true;
            displayManager = {
                startx.enable = true;
                lightdm.enable = mkForce false;
            };
            autorun = false;
        };
    };
}
{
    users = with j.attrs.users; let
        base = {
            hashedPassword = "$6$DoC/h6kR66Sa$aZKtTOXAqnan/jAC.4dH9tCYshheiKUZItR4g/kmMMLsfLQh0KslINL9zUTX2IjAZh9DE18eAh1AAz48.n/cm.";
            isNormalUser = true;
            createHome = true;
            extraGroups = [
                "wheel"
                "networkmanager"
                "persist"
                "libvirtd"
                "docker"
                "adbusers"
            ];
            openssh.authorizedKeys.keys = unique (flatten [
                (attrValues j.attrs.ssh.keys)
            ]);
        };
    in rec {
        users = mkMerge [
            (genAttrs j.attrs.allUsers (user: base))
            {
                "${primary}" = {
                    uid = 4362;
                    home = j.attrs.homes.${primary};
                    description = "Jeet Ray";
                    group = primary;
                    extraGroups = [ secondary ];
                    shell = pkgs.xonsh;
                };
                "${secondary}" = {
                    uid = 1111;
                    home = j.attrs.homes.${secondary};
                    description = "Alicia Summers";
                    group = secondary;
                    extraGroups = [ primary ];
                    shell = pkgs.fish;
                };
                "${nightingale}" = {
                    uid = 8888;
                    home = j.attrs.homes.${nightingale};
                    description = "Curtis Nightingale";
                    group = "root";
                    extraGroups = [ primary secondary ];
                    shell = pkgs.zsh;
                };
                root = {
                    shell = mkForce pkgs.zsh;
                    home = j.attrs.allHomes.root;
                    isNormalUser = mkForce false;
                    isSystemUser = mkForce true;
                };
            }
        ];

        mutableUsers = false;

        groups = {
            "${primary}" = {
                gid = config.users.users.${primary}.uid;
                members = [ primary secondary nightingale ];
            };
            "${secondary}" = {
                gid = config.users.users.${secondary}.uid;
                members = [ primary secondary nightingale ];
            };
            "${nightingale}" = {
                gid = config.users.users.${nightingale}.uid;
                members = [ nightingale ];
            };
        };
    };
}
{
    environment = {
        shellInit = ''
            export GPG_TTY="$(tty)"
            gpg-connect-agent /bye
            export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
            echo UPDATESTARTUPTTY | gpg-connect-agent
        '';
        systemPackages = with pkgs; [
            pinentry-curses
        ];
    };
    programs = {
        # Some programs need SUID wrappers, can be configured further or are
        # started in user sessions.
        # mtr.enable = true;
        gnupg.agent = {
            enable = true;
            enableSSHSupport = true;
            pinentryFlavor = "curses";
        };
        ssh.startAgent = ! config.programs.gnupg.agent.enableSSHSupport;
    };
    security.pam.enableSSHAgentAuth = true;
}
( mkIf config.variables.client {
    # For Yubikey SSH-GPG Authentication
    environment = {
        systemPackages = with pkgs; [
            pcsctools
        ];
    };
    security.pam = {
        yubico = {
            enable = true;
            debug = true;
            mode = "challenge-response";
        };
    };
    services = {
        udev.packages = with pkgs; [
            libu2f-host
            libyubikey
            yubikey-personalization
        ];
        pcscd.enable = true;
    };
})
]; }
