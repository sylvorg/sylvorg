inputs@{ config, pkgs, ... } : with builtins; let
    source = fetchGit {
        url = "https://github.com/shadowrylander/shadowrylander";
        ref = "master";
    };

    flake = let
        flakePath = "${source}/system/etc/nixos";
        lock = builtins.fromJSON (builtins.readFile "${flakePath}/flake.lock");
    in (import (
        fetchTarball {
            url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
            sha256 = lock.nodes.flake-compat.locked.narHash; }
        ) { src =  flakePath; }).defaultNix;

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

    integer-default-truths = lib.mapAttrs (
        n: v: v == 1
    ) (lib.filterAttrs (n: v: isInt v) stc-home);

    home-manager' = fetchGit {
        url = "https://github.com/nix-community/home-manager";
        ref = "master";
    };
    impermanence = fetchGit {
        url = "https://github.com/nix-community/impermanence";
        ref = "master";
    };
    hash = user: lib.j.hostName { stc = stc-home // { inherit user; }; };
    customInputs = inputs // { inherit stc lib sources; };
in
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
        (import (./. + "/configs/${stc.host}") customInputs)
        (import ./modules/variables.nix customInputs)
        (import ./modules/users.nix customInputs)
        (import "${home-manager'}/nixos")
        (import "${impermanence}/nixos.nix")
        (if (type == "pinebook") then (
            "${fetchGit {
                url = "https://github.com/shadowrylander/wip-pinebook-pro";
                ref = "master";
            }}/pinebook_pro.nix"
        ) else {})
        (if (type == "rpi") then (import ./devices/rpi (inputs // stc)) else {})
    ];
    config = {
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
            systemd-boot.enable = mkForce config.vars.bootPart;
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
    }
}
