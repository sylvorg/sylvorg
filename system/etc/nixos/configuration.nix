inputs@{ config, pkgs, ... } : let
    source = fetchGit {
        url = "https://github.com/${attrs.users.primary}/${attrs.users.primary}";
        ref = "master";
    };

    flake = (import (
        let
            flakePath = "${source}/system/etc/nixos";
            lock = builtins.fromJSON (builtins.readFile "${flakePath}/flake.lock");
        in fetchTarball {
            url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
            sha256 = lock.nodes.flake-compat.locked.narHash; }
        ) { src =  flakePath; }).defaultNix

    inherit (flake) lib;
    inherit (flake.legacyPackages) sources;

    stc = lib.j.attrs.default-stc // {
        device = "";
        host = "";
        type = "";
    };
    stc-home = stc // {
        nixos = 1;
        zfs = null;
    };

    integer-default-truths = mapAttrs (
        n: v: v == 1
    ) (filterAttrs (n: v: isInt v) stc-home);

    home-manager' = fetchGit {
        url = "https://github.com/nix-community/home-manager";
        ref = "master";
    };
    impermanence = fetchGit {
        url = "https://github.com/nix-community/impermanence";
        ref = "master";
    };
    hash = user: lib.j.hostName { stc = stc-home // { inherit user; }; };
in
with builtins;
with lib;
with j;
with stc;
with integer-default-truths;
if (
    device == "" || host == "" || type == "" || zfs == null
) then (abort "Sorry! The device, host, type, and zfs status must be set!") else {
    options = {
        vars = mkOption {
            default = mkDefault {  };
            type = with lib.types; attrsOf bool;
        };
    };
    imports = [
        (import (./. + "/configs/${stc.host}") (inputs // { inherit lib; }))
        (import "${home-manager'}/nixos")
        (import "${impermanence}/nixos.nix")
        (if (device == "pinebook") then (
            "${fetchGit {
                url = "https://github.com/shadowrylander/wip-pinebook-pro";
                ref = "master";
            }}/pinebook_pro.nix"
        ) else {})
        (if (device == "rpi") then (import ./devices/rpi (inputs // stc)) else {})
    ];
    config = let
        systemLink = {
            source = "${source}/system";
            recursive = true;
        };
        homeLink = {
            source = "${source}/home";
            recursive = true;
        };
    in {
        { config, lib, ... }: with builtins; with lib; with j; {
            options = {
                vars = mkOption {
                    default = mkDefault {  };
                    type = with lib.types; attrsOf bool;
                };
            };
            config.vars = {
                bootPart = mkDefault true;
                syncDevice = mkDefault false;
            } // (mapAttrs (
                n: v: mkDefault (if (isInt v) then (v == 1) else v)
            ) (default-stc // stc));
        }
        inputs@{ config, lib, pkgs, sources, ... }: with builtins; with lib; with j; {
            users = with attrs.users; let
                base = mkMerge [{
                        hashedPassword = "$6$DoC/h6kR66Sa$aZKtTOXAqnan/jAC.4dH9tCYshheiKUZItR4g/kmMMLsfLQh0KslINL9zUTX2IjAZh9DE18eAh1AAz48.n/cm.";
                        isNormalUser = true;
                        createHome = true;
                        extraGroups = [
                            "wheel"
                            "networkmanager"
                            "persist"
                        ];
                        openssh.authorizedKeys.keys = [
                            attrs.ssh.keys.master
                        ];
                        packages = ../packages.nix inputs;
                    }
                    (mkIf (!config.vars.minimal) {
                        extraGroups = [ "libvirtd" "docker" ];
                    })
                ];
            in rec {
                users = mkMerge [
                    (genAttrs attrs.allUsers (user: base))
                    {
                        "${primary}" = {
                            uid = 4362;
                            home = attrs.allHomes.${primary};
                            description = "Jeet Ray";
                            group = primary;
                            extraGroups = [ secondary ];
                            shell = pkgs.xonsh;
                        };
                        "${secondary}" = {
                            uid = 1111;
                            home = attrs.allHomes.${secondary};
                            description = "Alicia Summers";
                            group = secondary;
                            extraGroups = [ primary ];
                            shell = pkgs.fish;
                        };
                        "${nightingale}" = {
                            uid = 8888;
                            home = attrs.allHomes.${nightingale};
                            description = "Curtis Nightingale";
                            group = "root";
                            extraGroups = [ primary secondary ];
                            shell = pkgs.zsh;
                        };
                        root = {
                            shell = mkForce pkgs.xonsh;
                            home = attrs.allHomes.root;
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
        home-manager = {
            useUserPackages = true;
            useGlobalPkgs = true;
            backupFileExtension = "bak";
            verbose = true;

            # TODO
            sharedModules = [
                {  }
            ];

            extraSpecialArgs = flake.legacyPackages.make.specialArgs { inherit stc; };

            users = listToAttrs (map (user: nameValuePair user (let
                homeDirectory = attrs.allHomes.${user};
            in {
                imports = [ (import "${impermanence}/home-manager.nix") ];
                programs.home-manager = {
                    enable = true;
                    path = home-manager';
                };
                home = {
                    file = mkMerge [{
                            "${attrs.users.primary}".source = source;
                            
                        }
                        (myIf.set (user == "root") (attrs.link "/" "${source}/system"))
                        (attrs.link homeDirectory "${source}/home")
                    ];
                    ${myIf.knull (nixos && zfs) "persistence"} = attrs.persistence.home {  };
                };
            })) attrs.allUsers);
        };
        environment = {
            ${myIf.knull zfs "persistence"} = attrs.persistence.system {  };
            etc."nix/nix.conf".text = attrs.configs.nix;
        };
        nixpkgs = {
            overlays = import ./overlays { inherit stc; };
            config = {
                allowUnfree = true;
                allowBroken = true;
                allowUnsupportedSystem = true;
                # preBuild = ''
                #     makeFlagsArray+=(CFLAGS="-w")
                #     buildFlagsArray+=(CC=cc)
                # '';
                permittedInsecurePackages = [
                    "python2.7-cryptography-2.9.2"
                ];
            };
        };
        boot.loader = {
            systemd-boot.enable = mkForce (if (config.vars ? bootPart) then config.vars.bootPart else true);
            efi.efiSysMountPoint = "/boot/efi";
            efi.canTouchEfiVariables = mkForce true;
            grub.efiInstallAsRemovable = mkForce false;
            grub.enable = mkForce false;
            grub.efiSupport = mkForce true;
            grub.device = mkForce "nodev";
            grub.version = mkForce 1;
            grub.copyKernels = mkForce true;
            grub.forceInstall = mkForce true;
            grub.zfsSupport = mkForce false;

            # Used for Bedrock Linux
            initScript.enable = mkForce true;
        };
        nix = {
            package = pkgs.nixUnstable;
            extraOptions = '' experimental-features = nix-command flakes '';
        };
        environment.systemPackages = with pkgs; [ vim git rsync tmux byobu xonsh yadm
            # python39
        ];
    };
}
