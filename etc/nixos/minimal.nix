with builtins; args@{ config, system ? currentSystem ... }:
let
flake = import ./.;
inherit (flake) lib overlays make;
nixpkgset = make.nixpkgset overlays system lib;
pkgs = make.pkgs nixpkgset;
dir = "${lib.j.attrs.homes.${lib.j.attrs.users.primary}}/.local/share/yadm/repo.git";
dirExists = pathExists dir;
repo = with lib; j.functions.mntConvert (if dirExists then (fetchGit { url = "file://${dir}"; ref = "main"; }) else flake.${j.attrs.users.primary})
configuration = import <nixpkgs/nixos> { configuration.imports = [ ./configuration.nix ]; };
hardware-configuration = import <nixpkgs/nixos> { configuration.imports = [
    ./hardware-configuration.nix
    ({config, ... }: { networking.hostId = "16f3d1b9"; boot.loader.grub.devices = [ "nodev" ]; })
]; };
in with lib; {
imports = flatten [ flake.home-manager.nixosModules.home-manager impermanence.nixosModules.impermanence ];
config = (removeAttrs hardware-configuration.config [ "fileSystems" "nesting" "jobs" "fonts" "meta" "documentation" ]) // {
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
persistence = let
    rootDirSet = {
        user = "root";
        group = "root";
    };
    rootFileSet.parentDirectory = rootDirSet;
in {
    "/persist/root" = {
        directories = unique (map (directory: if ((typeOf directory) == "string") then ({ inherit directory; } // rootDirSet) else (rootDirSet // directory)) (flatten [
            "/etc/nix"
            "/etc/nixos"
            "/etc/zsh"
        ]));
    };
    "/persist" = let
        redRepo = readDir repo;
        redRepoFiles = flatten [
            (attrNames (filterAttrs (n: v: v != "directory") redRepo))
            [ "configuration.nix" "hardware-configuration.nix" "datasets.nix" ]
        ];
        redRepoDirectories = flatten [
            (attrNames (filterAttrs (n: v: v == "directory") redRepo))
        ];
    in {
        hideMounts = true;
        files = unique (map (file: if ((typeOf file) == "string") then ({ inherit file; } // rootFileSet) else (rootFileSet // file)) (flatten [
            "/etc/host"
            "/etc/machine-id"
        ]));
        directories = unique (map (directory: if ((typeOf directory) == "string") then ({ inherit directory; } // rootDirSet) else (rootDirSet // directory)) (flatten [
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
        ]));
        users = listToAttrs (map (user: let
            userDirSet = {
                inherit user;
                group = user;
            };
            userFileSet.parentDirectory = userDirSet;
        in nameValuePair user {
            home = j.attrs.allHomes.${user};
            files = unique (map (file: if ((typeOf file) == "string") then ({ inherit file; } // userFileSet) else (userFileSet // file)) (flatten [
                ".bash-history"
                ".emacs-profile"
                ".gitignore"
                ".globalignore"
                ".nix-channels"
                ".python-history"
                ".viminfo"
                ".zsh-history"
                ".screenrc"
                redRepoFiles
            ]));
            directories = unique (map (directory: if ((typeOf directory) == "string") then ({ inherit directory; } // userDirSet) else (userDirSet // directory)) (flatten [
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
                redRepoDirectories
            ]));}) j.attrs.allUsers);
    };
};
};
zramSwap = {
    enable = true;
    algorithm = "zstd";
};
fileSystems = let
    inherit (j.attrs.fileSystems) base;
    fileSystems' = j.attrs.datasets.fileSystems;
in (filterAttrs (n: v: ! (builtins.elem "bind" v.options)) hardware-configuration.config.fileSystems) // (mapAttrs' (dataset: mountpoint: nameValuePair mountpoint (
    mkForce (base // { device = dataset; ${
        j.functions.myIf.knull ((hasInfix j.attrs.users.primary dataset) || (hasInfix "persist" dataset)) "neededForBoot"
    } = true; })
)) fileSystems');
hardware = {
    enableRedistributableFirmware = lib.mkDefault true;
    # Enable sound
    pulseaudio.enable = true;
};
sound.enable = true;

# TODO: This isn't working because configuration.nix imports this file, i.e. super.nix, which then again imports configuration.nix, and so on.
# networking = (mapAttrs (n: v: v // { wakeOnLan.enable = true; }) configuration.config.networking.interfaces) // {

networking = {
    # interfaces = map (interface:
    #     { inherit interface; method = "magicpacket"; }
    # ) (attrNames config.networking.interfaces);
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
    extraOptions = j.attrs.configs.nix;
    settings = {
        auto-optimise-store = true;
        sandbox = true;
    };
    # sandboxPaths = [];
};
nixpkgs = nixpkgset;
services.logind.lidSwitch = "hybrid-sleep";
powerManagement = {
    enable = true;
    cpuFreqGovernor = mkForce "ondemand";
};
programs = {
    xonsh.enable = true;
    fish.enable = true;
    zsh.enable = true;
    extra-container.enable = true;
};
services = {
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
                shell = if (!elem system [ "aarch64-linux" ]) then pkgs.fish else pkgs.zsh;
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
};
}
