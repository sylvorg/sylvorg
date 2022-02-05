{ config, ... }: with builtins;
let
ref = "master";
nixpkgs = fetchGit { url = "https://github.com/nixos/nixpkgs"; inherit ref; };
prepkgs = import nixpkgs {  };
lib = prepkgs.lib.extend (final: prev: { j = import ./lib.nix prepkgs final config.networking.hostName; });
overlays = import ./overlays.nix lib nixpkgs pkgs ref;
pkgs = import nixpkgs { inherit overlays; };
in with lib; {
imports = [
    ./hardware-configuration.nix
    "${fetchGit { url = "https://github.com/nix-community/impermanence"; }}/nixos.nix"
    "${fetchGit { url = "https://github.com/${j.attrs.users.primary}/nixpkgs"; ref = "guix"; }}/nixos/modules/services/development/guix.nix"
];
boot = {
supportedFilesystems = j.attrs.fileSystems.supported;
initrd = {
    inherit (config.boot) supportedFilesystems;
    compressor = "${lib.getBin pkgs.zstd}/bin/zstd";
    network.ssh.enable = true;
};
extraModprobeConfig = '' options kvm_intel_nested=1 '';
loader = {
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
        # devices = [ "nodev" ];
        device = "nodev";
        version = 2;

        # TODO: Get more options
        extraEntries = ''
            menuentry "Reboot" { reboot }
            menuentry "Poweroff" { halt }
        '';

    };
    efi = {
        canTouchEfiVariables = mkForce true;
        efiSysMountPoint = "/boot/efi";
    };
    timeout = 10;

    # Used for Bedrock Linux
    # Also causing EFI stuff not to be installed
    # initScript.enable = mkForce true;

};
# kernelPackages = mkDefault pkgs.linuxPackages_xanmod;
# kernelPackages = mkDefaultpkgs.linuxPackages_lqx;
# kernelPackages = mkDefaultpkgs.linuxPackages_zen;
kernelPatches = [
];
extraModulePackages = with config.boot.kernelPackages; [
    # anbox
    # wireguard
    zfsUnstable
];
binfmt.emulatedSystems = [
    "armv7l-linux"
    "aarch64-linux"
];
kernelModules = [ "zfs" ];
kernelParams = [ "nohibernate" ];
# loader.grub.zfsSupport = true;
initrd = {
    postDeviceCommands = mkAfter ''
        zfs rollback -r ${config.networking.hostName}/system/root@blank
        zfs rollback -r ${config.networking.hostName}/system/home@blank
        zfs rollback -r ${config.networking.hostName}/system/tmp@blank
    '';
    kernelModules = [ "zfs" ];
    availableKernelModules = [ "zfs" ];
};
zfs = {
    requestEncryptionCredentials = true;
    enableUnstable = true;
    devNodes = "/dev/";
};
};
console = {
    # Select internationalisation properties.
    # font = lib.mkDefault "${terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    font = "Cartograph CF Extra Bold Italic";
    keyMap = "us";
};
environment = {
etc."nix/nix.conf".text = mkForce j.attrs.configs.nix;
pathsToLink = [ "/share/nix-direnv" ];
systemPackages = with pkgs; [
    #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
] ++ (map (pkg: pkgs.gnome."gnome-${pkg}") [
    # "boxes"
    # "characters"
    # "tweaks"
    "session"
]) ++ (with pkgs.gnome; [
    # dconf-editor
]);
persistence = let
    rootDirSet = {
        user = "root";
        group = "root";
    };
    rootFileSet.parentDirectory = rootDirSet;
in {
    # "/root" = {
    #     directories = map (directory: if ((typeOf directory) == "string") then ({ inherit directory; } // rootSet) else (rootDirSet // directory)) (flatten [
    #         [
    #             "/etc/nix"
    #             "/etc/nixos"
    #             "/etc/zsh"
    #         ]
    #     ]);
    # };
    "/persist" = let
        dir = "${j.attrs.homes.${j.attrs.users.primary}}/.local/share/yadm/repo.git";
        repo = fetchGit {
            url = if (pathExists dir) then "file://${dir}" else "https://github.com/${j.attrs.users.primary}/${j.attrs.users.primary}"; };
        redRepo = readDir repo;
        redRepoFiles = flatten [
            (attrNames (filterAttrs (n: v: v != "directory") redRepo))
            [ "configuration.nix" "hardware-configuration.nix" ]
        ];
        redRepoDirectories = flatten [
            (attrNames (filterAttrs (n: v: v == "directory") redRepo))
        ];
    in {
        hideMounts = true;
        files = map (file: if ((typeOf file) == "string") then ({ inherit file; } // rootFileSet) else (rootFileSet // file)) (flatten [
            [
                "/etc/host"
                "/etc/machine-id"
            ]
        ]);
        directories = map (directory: if ((typeOf directory) == "string") then ({ inherit directory; } // rootSet) else (rootDirSet // directory)) (flatten [
            [
                "/bin"
                "/etc/containers"
                "/etc/NetworkManager/system-connections"
                "/etc/ssh"
                "/etc/wireguard"
                "/sbin"
                "/snap"
                "/usr"
                "/var/lib/acme"
                "/var/lib/bluetooth"
                "/var/lib/systemd/coredump"
                "/var/log"
            ]
        ]);
        users = listToAttrs (map (user: let
            userDirSet = {
                inherit user;
                group = user;
            };
            userFileSet.parentDirectory = userDirSet;
        in nameValuePair user {
            files = map (file: if ((typeOf file) == "string") then ({ inherit file; } // userFileSet) else (userFileSet // file)) (flatten [
                [
                    ".bash-history"
                    ".emacs-profile"
                    ".gitignore"
                    ".globalignore"
                    ".nix-channels"
                    ".python-history"
                    ".viminfo"
                    ".zsh-history"
                    ".screenrc"
                ]
                redRepoFiles
            ]);
            directories = map (directory: if ((typeOf directory) == "string") then ({ inherit directory; } // userSet) else (userDirSet // directory)) (flatten [
                [
                    ".atom"
                    ".byobu"
                    ".cache"
                    ".caddy"
                    ".config"
                    ".linuxbrew"
                    ".local"
                    ".mozilla"
                    ".peru"
                    ".pki"
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
                ]
                redRepoDirectories
            ])}) allUsers);
    };
};
};
fileSystems = let
    inherit (j.attrs.fileSystems) base;
    fileSystems' = j.attrs.datasets.fileSystems;
in mapAttrs' (dataset: mountpoint: nameValuePair mountpoint (
    mkForce (base // { device = dataset; ${
        j.functions.myIf.knull ((hasInfix j.attrs.users.primary dataset) || (hasInfix "persist" dataset)) "neededForBoot"
    } = true; })
)) fileSystems';
hardware = {
    enableRedistributableFirmware = lib.mkDefault true;
    # Enable sound
    pulseaudio.enable = true;
};
sound.enable = true;
zramSwap = {
    enable = true;
    algorithm = "zstd";
};
xdg.portal.enable = mkForce (!elem currentSystem [ "aarch64-linux" ]);
i18n = {
    # Select internationalisation properties.
    defaultLocale = "en_US.UTF-8";
};
time.timeZone = "America/Toronto";
system = {
    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It's perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    # stateVersion = "20.09"; # Did you read the comment?
    autoUpgrade = {
        enable = true;
        allowReboot = false;
        flake = https://github.com/nixos/nixpkgs/archive/master.tar.gz;
    };
};
networking = let
    wofie = "4876d858001ae2b6b27f7517f36045a09836fb877cbafef3b101e5c995af7a71";
in {
    # interfaces = map (interface:
    #     { inherit interface; method = "magicpacket"; }
    # ) (attrNames config.networking.interfaces);
    wireless = {
        # enable = true; # Enables wireless support via wpa_supplicant.
        enable = false; # Enables wireless support via wpa_supplicant.
        networks = {
            "Wofie" = {
                pskRaw = wofie;
                priority = 0;
            };
        };
    };
    networkmanager.enable = mkForce true;

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;

    # Configure network proxy if necessary
    # proxy = {
    # default = "http://user:password@proxy:port/";
    # noProxy = "127.0.0.1,localhost,internal.domain";
    # };

    # Or disable the firewall altogether.
    # enable = false;
    # };

    wireguard.interfaces.wg0 = {
        generatePrivateKeyFile = true;
        privateKeyFile = "/persist/etc/wireguard/wg0";
    };

    # hostId = substring 0 8 (readFile "/etc/machine-id");

    firewall = mkIf (elem config.networking.hostName j.attrs.relays) {
        allowedTCPPorts = [ 22 80 222 443 2022 8080 9418 ];
        allowedUDPPortRanges = [
            {
                from = 60000;
                to = 61000;
            }
        ];
    };
};
nix = rec {
    gc = j.functions.foldToSet [
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
    autoOptimiseStore = true;
    extraOptions = j.attrs.configs.nix;
    useSandbox = true;
    # sandboxPaths = [];
};
nixpkgs = { inherit overlays; };
services.logind.lidSwitch = "hybrid-sleep";
powerManagement = {
    enable = true;
    cpuFreqGovernor = mkForce "ondemand";
};
programs = {
    xonsh.enable = true;
    fish.enable = true;
    zsh.enable = true;
};
# For Yubikey SSH-GPG Authentication
environment.shellInit = ''
    export GPG_TTY="$(tty)"
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
    echo UPDATESTARTUPTTY | gpg-connect-agent
'';
programs = {
    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # mtr.enable = true;
    gnupg.agent = {
        enable = true;
        enableSSHSupport = false;
        pinentryFlavor = "curses";
    };

    # For use with Yubikey SSH-GPG Authentication, set to false
    ssh.startAgent = true;
};
security.pam = {
    yubico = {
        enable = true;
        debug = true;
        mode = "challenge-response";
    };
    enableSSHAgentAuth = true;
};
services = {
flatpak.enable = !elem currentSystem [ "aarch64-linux" ];
guix.enable = true;
printing.enable = true;
openssh = {
    enable = true;
    extraConfig = mkOrder 0 ''
        TCPKeepAlive yes
        ClientAliveCountMax 480
        ClientAliveInterval 3m
    '';
    permitRootLogin = "yes";
};
udev.extraRules = ''
    ACTION=="add|change", KERNEL=="sd[a-z]*[0-9]*|mmcblk[0-9]*p[0-9]*|nvme[0-9]*n[0-9]*p[0-9]*", ENV{ID_FS_TYPE}=="zfs_member", ATTR{../queue/scheduler}="none"
''; # zfs already has its own scheduler. without this my(@Artturin) computer froze for a second when i nix build something.
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
udev.packages = with pkgs; [
    yubikey-personalization
    libu2f-host
];
pcscd.enable = true;
zfs = {
    trim.enable = true;
    autoScrub.enable = true;

    # Managed by Sanoid
    autoSnapshot.enable = false;
};
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

        # 6 snapshots an hour
        daily = 144;

        # 2 snapshots a minute
        hourly = 120;

        # 6 snapshots a day for 28 days
        monthly = 168;

        # Twice the weeks in a year
        yearly = 104;
    };

    datasets = listToAttrs (map (dataset: nameValuePair "${config.networking.hostName}/${dataset}" sanoidBase) (flatten [
        j.attrs.datasets.backup
        [ config.networking.hostName ]
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
    sshKey = "/root/.ssh/id_ecdsa";
    commands = listToAttrs (map (dataset: nameValuePair "${host}/${dataset}" (syncoidBase // { target = ""; })) (flatten [
        j.attrs.datasets.backup
        [ config.networking.hostName ]
    ]));
};
};
systemd = {
    # packages = with pkgs; [ runit ];
    services = {
        # runit.enable = true;
        caddy = mkIf (elem config.networking.hostName j.attrs.relays) (j.attrs.configs.services.base // {
            serviceConfig = {
                ExecStart = ''
                    ${pkgs.caddy}/bin/caddy run --config ${j.attrs.homes.${j.attrs.users.primary}}/.config/caddy/files/${config.networking.hostName} --adapter yaml 2>&1
                '';
                ExecStop = ''
                    pkill caddy
                '';
            };
        });
        emacs = j.attrs.configs.services.base // rec {
            description = "Emacs text editor";
            documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
            serviceConfig = {
                ExecStart = ''
                    ${pkgs.emacs}/bin/emacs --bg-daemon=damascus --update
                '';
                ExecStop = ''
                    ${pkgs.emacs}/bin/emacsclient -s damascus -e "(kill-emacs)"
                '';
                Type = "forking";
                TimeoutSec = 900;
            };
            environment = {
                SSH_AUTH_SOCK = "%t/keyring/ssh";
            };
        };
    };
};
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
        ];
        openssh.authorizedKeys.keys = [
            j.attrs.ssh.keys.master
        ];
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
                shell = pkgs.zsh;
            };
            "${secondary}" = {
                uid = 1111;
                home = j.attrs.homes.${secondary};
                description = "Alicia Summers";
                group = secondary;
                extraGroups = [ primary ];
                shell = if (!elem currentSystem [ "aarch64-linux" ]) then pkgs.fish else pkgs.zsh;
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
virtualisation = {
    xen.enable = false;
    lxd.zfsSupport = true;
    podman.enable = true;
    docker = {
        enable = true;
        storageDriver = "zfs";
        package = pkgs.docker;
        enableOnBoot = true;
    };
    libvirtd.enable = true;
};
}
