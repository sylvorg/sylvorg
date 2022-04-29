with builtins; args@{ config, ... }:
let
flake = import ./.;
system = args.system or currentSystem;
host = args.host or config.networking.hostName;
fromFlake = args ? inputs;
inheritanceSet = if fromFlake then args else (flake.make.specialArgs host system);
inherit (inheritanceSet) lib overlays nixpkgset pkgs;
dir = "${lib.j.attrs.homes.${lib.j.attrs.users.primary}}/.local/share/yadm/repo.git";
dirExists = pathExists dir;
repo = with lib; j.functions.mntConvert (if dirExists then (fetchGit { url = "file://${dir}"; ref = "main"; }) else flake.inputs.${j.attrs.users.primary});
nixos = "${(args.nixpkgs or <nixpkgs>)}/nixos";
nixos-configuration = configuration: import nixos { configuration = import configuration (args // inheritanceSet); inherit system; };
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
    (if fromFlake then [] else [ home-manager.nixosModules.home-manager impermanence.nixosModules.impermanence ])
];
# config = (removeAttrs nixos-configurations.hardware-configuration.config [ "fileSystems" "nesting" "jobs" "fonts" "meta" "documentation" ]) // {
config = mkMerge [
    (filterAttrs (n: v: elem n [ "boot" "networking" "powerManagement" "hardware" ]) nixos-configurations.hardware-configuration.config)

    # TODO: What exactly from `system' am I taking? Merge it explicitly.
    # (if fromFlake then (filterAttrs (n: v: elem n [ "system" ]) nixos-configurations.configuration.config) else {})

{
boot = {
supportedFilesystems = j.attrs.fileSystems.supported;
initrd = {
    inherit (config.boot) supportedFilesystems;
    compressor = "${lib.getBin pkgs.zstd}/bin/zstd";
    network.ssh.enable = true;
};
extraModprobeConfig = '' options kvm_intel_nested=1 '';
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
(if (elem "bcachefs" config.boot.supportedFilesystems) then (filter (set: hasInfix "bcachefs" set.name) pkgs.linuxKernel.kernels.linux_testing_bcachefs.kernelPatches) else [])
];
extraModulePackages = with config.boot.kernelPackages; [
    zfsUnstable
];
kernelModules = [ "zfs" ];
kernelParams = [ "nohibernate" ];
# loader.grub.zfsSupport = mkIf j.attrs.zfs true;
initrd = {
    postDeviceCommands = mkIf j.attrs.zfs (mkAfter (concatMapStrings (d: "zfs rollback -r ${d}@blank\n") (filter (d: (j.functions.hasAPrefix [
        "${host}/system/home"
    ] d) || (elem d [
        "${host}/system/root"
        # "${host}/system/tmp"
    ])) (attrNames j.attrs.datasets.fileSystems))));
    kernelModules = [ "zfs" "r8169" ];
    availableKernelModules = config.boot.initrd.kernelModules;
};
zfs = mkIf j.attrs.zfs {
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
systemPackages = with pkgs; flatten [
    cachix
    pkgs.j.pkgs.j.xonsh.buildInputs
];
persistence = mkIf j.attrs.zfs (let
    rootDirSet = {
        user = "root";
        group = "root";
    };
    rootFileSet.parentDirectory = rootDirSet;
in {
    "/persist/root" = {
        hideMounts = true;
        files = unique (map (file: if (isString file) then ({ inherit file; } // rootFileSet) else (rootFileSet // file)) (flatten [
            "/etc/host"
            "/etc/machine-id"

            # (map (directory: map (fd: "/${directory}/${fd}") (attrNames (filterAttrs (n: v: v != "directory") (let path = "${repo}/${directory}"; in j.functions.readDirExists path)))) [
            #     "etc"
            #     "var"
            # ])

            (map (directory: j.functions.recurseDir { dir = "${repo}/${directory}"; local = 0; ignores.prefix = [ "nixos/" ]; }) [
                "etc"
                "var"
            ])
        ]));
        directories = unique (map (directory: if (isString directory) then ({ inherit directory; } // rootDirSet) else (rootDirSet // directory)) (flatten [
            "/bin"
            "/etc/containers"
            "/etc/NetworkManager/system-connections"
            "/etc/nixos"

            # TODO: Prevents `sshd_config' itself from being created
            # "/etc/ssh"

            "/sbin"
            "/snap"
            "/usr"
            "/var/lib/acme"
            "/var/lib/bluetooth"
            "/var/lib/systemd/coredump"
            "/var/log"

            # (map (directory: map (fd: "/${directory}/${fd}") (attrNames (filterAttrs (n: v: v == "directory") (let path = "${repo}/${directory}"; in j.functions.readDirExists path)))) [
            #     "etc"
            #     "var"
            # ])
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
            predRepo = let pHome = "/persist/${home}"; in j.functions.readDirExists pHome;
            predRepoFiles = flatten [
                (attrNames (filterAttrs (n: v: v != "directory") predRepo))
            ];
            predRepoDirectories = flatten [
                (attrNames (filterAttrs (n: v: v == "directory") predRepo))
            ];
        in nameValuePair user {
            inherit home;
            files = unique (map (file: if (isString file) then ({ inherit file; } // userFileSet) else (userFileSet // file)) (flatten [
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
                redRepoFiles
            ]));
            directories = unique (map (directory: if (isString directory) then ({ inherit directory; } // userDirSet) else (userDirSet // directory)) (flatten [
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
                ".repos"
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
});
};
zramSwap = {
    enable = true;
    algorithm = "zstd";
};
fileSystems = let
    inherit (j.attrs.fileSystems) base;
in mkMerge [
    # (filterAttrs (n: v: !elem "bind" v.options) nixos-configurations.hardware-configuration.config.fileSystems)
    (filterAttrs (n: v: elem n [ "/boot" "/boot/efi" ]) nixos-configurations.hardware-configuration.config.fileSystems)
    (mkIf j.attrs.zfs (mapAttrs' (dataset: mountpoint: nameValuePair mountpoint (
        mkForce (base // { device = dataset; ${
            j.functions.myIf.knull ((j.functions.hasAnInfix [
                j.attrs.users.primary
                "persist"
                "home"
            ] dataset) || (elem dataset [ ])) "neededForBoot"
        } = true; })
    )) j.attrs.datasets.fileSystems))
];
hardware = {
    enableRedistributableFirmware = lib.mkDefault true;
    # Enable sound
    pulseaudio.enable = true;
};
sound.enable = true;
networking = {
    networkmanager.enable = mkForce true;
    interfaces = if fromFlake then (mapAttrs (n: v: v // {
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

    # Or disable the firewall altogether.
    # enable = false;
    # };

    wireguard.interfaces.wg0 = {
        generatePrivateKeyFile = true;
        privateKeyFile = "/persist/etc/wireguard/wg0";
    };

    firewall = mkIf j.attrs.relay {
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
    tmux = {
        enable = true;
        extraConfig = ''
            
            # source ./resources/powerline/powerline/bindings/tmux/powerline-base.conf
            source ./resources/powerline/powerline/bindings/tmux/powerline.conf
            # source ./resources/powerline/powerline/bindings/tmux/powerline_tmux_2.1_plus.conf
            source ./resources/oh-my-tmux/.tmux.conf
            source ./resources/oh-my-tmux/.tmux.conf.local
            unbind C-b
            unbind C-x
            unbind C-z
            bind-key 
            bind-key -n 
            bind-key -nr M-s send-prefix
            bind-key -nr C-S-F5 send-keys M-F5
            bind-key -nr C-S-Left send-keys M-Left
            bind-key -nr C-S-Right send-keys M-Right
            bind-key -r 
            bind-key -T prefix 
            set -g prefix2 S-Space
            
            # Mouse support - set to on if you want to use the mouse
            # setw -g mode-mouse on
            # set -g mouse-select-pane on
            # set -g mouse-resize-pane on
            # set -g mouse-select-window on
            
            # enable activity alerts
            setw -g monitor-activity off
            set -g visual-activity off
            
            # Center the window list
            set -g status-justify centre
            
            # VI Mode
            set -g status-keys vi
            
            # utf8 is on
            # set -g utf8 on
            # set -g status-utf8 on
            
            run-shell "powerline-daemon -q"
            
            set -g status-right '#{prefix_highlight} | %a %Y-%m-%d %H:%M'
            
            # Spacemacs Settings:
            set -gs escape-time 10
            
            # address vim mode switching delay (http://superuser.com/a/252717/65504)
            set -s escape-time 0
            
            # increase scrollback buffer size
            set -g history-limit 50000
            
            # tmux messages are displayed for 1.25 seconds
            set -g display-time 1250
            
            # refresh 'status-left' and 'status-right' more often
            # set -g status-interval 1
            
            # focus events enabled for terminals that support them
            set -g focus-events on
            
            # border thickness
            set-option -g pane-active-border-style "bg=default"
            set-option -ag pane-active-border-style "fg=colour208"
            set -g status-right '#{prefix_highlight} | %a %Y-%m-%d %H:%M'
            
            # Adapted From: https://www.reddit.com/r/tmux/comments/einuqy/make_tmux_modal/
            set-option -g prefix None
            bind-key -n C-Space {
              set-option key-table prefix
              set-option status-bg yellow
            }
            bind-key -T prefix C-Space {
              set-option key-table root
              set-option status-bg green
            }
            
            setw -g aggressive-resize on
            set -g @plugin tmux-plugins/tpm
            set -g @plugin tmux-plugins/tmux-battery
            set -g @plugin tmux-plugins/tmux-cpu
            set -g @plugin tmux-plugins/tmux-fpp
            set -g @plugin tmux-plugins/tmux-logging
            set -g @plugin tmux-plugins/tmux-online-status
            set -g @plugin tmux-plugins/tmux-open
            set -g @plugin tmux-plugins/tmux-pain-control
            set -g @plugin tmux-plugins/tmux-prefix-highlight
            set -g @plugin tmux-plugins/tmux-sessionist
            set -g @plugin tmux-plugins/tmux-sidebar
            set -g @plugin tmux-plugins/vim-tmux-focus-events
            set -g @plugin tmux-plugins/tmux-yank
            set -g @plugin tmux-plugins/tmux-continuum
            set -g @plugin tmux-plugins/tmux-resurrect
            set -g @plugin christoomey/vim-tmux-navigator
            set -g @plugin sainnhe/tmux-fzf
            set -g @plugin samoshkin/tmux-plugin-sysstat
            set -g @plugin wfxr/tmux-fzf-url
            set -g @plugin schasse/tmux-jump
            set -g @plugin eraserhd/tmux-ctrlw
            set -g @plugin jlipps/tmux-safekill
            set -g @plugin fcsonline/tmux-thumbs
            set -g @plugin addisonlynch/tmux-sidebar-plus
            set -g @continuum-boot 'on'
            set -g @continuum-save-interval '10'
            set -g @continuum-restore 'on'
            set -g @resurrect-save-bash-history 'on'
            set -g @resurrect-dir '~/.byobu/tmux_resurrect'
            set -g @resurrect-processes '"mc --nocolor" "tail -f" bat docker elvish emacs fish glances gotop htop ipython irssi jupyter-lab jupyter-notebook less man more mosh mutt nvim ssh syncthing tail top vi vim weechat wtf xonsh xsh zsh'
            set -g @resurrect-capture-pane-contents 'on'
            set -g @resurrect-strategy-vim 'session'
            set -g @resurrect-strategy-nvim 'session'
            run "./resources/tpm/tpm"
        '';
    };
};
services = {
openssh = mkForce nixos-configurations.server.config.services.openssh;
udev.extraRules = mkIf j.attrs.zfs ''
    ACTION=="add|change", KERNEL=="sd[a-z]*[0-9]*|mmcblk[0-9]*p[0-9]*|nvme[0-9]*n[0-9]*p[0-9]*", ENV{ID_FS_TYPE}=="zfs_member", ATTR{../queue/scheduler}="none"
''; # zfs already has its own scheduler. without this my(@Artturin) computer froze for a second when i nix build something.
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
    # For Yubikey SSH-GPG Authentication
    environment = {
        shellInit = ''
            export GPG_TTY="$(tty)"
            gpg-connect-agent /bye
            export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
            echo UPDATESTARTUPTTY | gpg-connect-agent
        '';
        systemPackages = with pkgs; [ pinentry-curses ];
    };
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
        udev.packages = with pkgs; [
            libu2f-host
            libyubikey
            yubikey-personalization
        ];
        pcscd.enable = true;
    };
}
];
}
