{
    nixConfig = {
        extra-substituters = "https://cache.nixos.org https://nix-community.cachix.org";
        extra-trusted-public-keys = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
        # keep-derivations = true;
        # keep-outputs = true;
        extra-experimental-features = "nix-command flakes";
        # allow-unsafe-native-code-during-evaluation = true;
        # min-free = 262144000;
        # max-free = 1073741824;
    };

    inputs = rec {
        # nixos-unstable.url = github:NixOS/nixpkgs/nixos-unstable;
        nixos-unstable.url = "https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz";

        # nixos-unstable-small.url = github:NixOS/nixpkgs/nixos-unstable-small;
        nixos-unstable-small.url = "https://nixos.org/channels/nixos-unstable-small/nixexprs.tar.xz";

        # nixos-21-11.url = github:NixOS/nixpkgs/nixos-21.11;
        nixos-21-11.url = "https://nixos.org/channels/nixos-21.11/nixexprs.tar.xz";

        # nixos-21-11-small.url = github:NixOS/nixpkgs/nixos-21.11-small;
        nixos-21-11-small.url = "https://nixos.org/channels/nixos-21.11-small/nixexprs.tar.xz";

        release-21-11.url = github:NixOS/nixpkgs/release-21.11;

        master.url = github:NixOS/nixpkgs/master;

        j.url = github:shadowrylander/nixpkgs/j;

        nixpkgs.follows = "j";

        hardware.url = github:nixos/nixos-hardware;

        shadowrylander = {
            url = github:shadowrylander/shadowrylander;
            flake = false;
        };

        flake-compat = {
            url = github:edolstra/flake-compat;
            flake = false;
        };

        home-manager.url = github:nix-community/home-manager;
        impermanence.url = github:nix-community/impermanence;

        nix.url = github:nixos/nix;
        extra-container.url = github:erikarvstedt/extra-container;
        nur.url = github:nix-community/nur;
        emacs.url = github:nix-community/emacs-overlay;
        mozilla.url = github:mozilla/nixpkgs-mozilla;

        flake-utils.url = github:numtide/flake-utils;
    };

    outputs = inputs@{ self, nixpkgs, flake-utils, ... }: with builtins; with nixpkgs.lib; with flake-utils.lib; let
        channel = "j";
        make = {
            lib = host: system: nixpkgs.lib.extend (final: prev: {
                j = import ./lib.nix ({ pkgs = import nixpkgs { inherit system; }; lib = final; inherit inputs; } // (if (host == null) then {} else { inherit host; }));
            });
            pre-nixpkgset = system: lib: { inherit system; config = lib.j.attrs.configs.nixpkgs; };
            overlays = system: lib: import ./overlays.nix {
                inherit lib nixpkgs inputs channel;
                pkgs = mapAttrs (n: v: import v (make.pre-nixpkgset system lib)) (
                    (filterAttrs (n: v: (hasPrefix "nixos-" n) || (hasPrefix "release-" n)) inputs) //
                    (with inputs; { inherit master j; })
                );
            };
            nixpkgset = overlays: system: lib: { inherit overlays system; config = lib.j.attrs.configs.nixpkgs; };
            pkgs = nixpkgset: import nixpkgs nixpkgset;
            specialArgs = name: system: rec {
                inherit inputs nixpkgs system;
                host = name;
                lib = make.lib name system;
                pre-nixpkgset = make.pre-nixpkgset system lib;
                overlays = make.overlays system lib;
                nixpkgset = make.nixpkgset overlays system lib;
                pkgs = make.pkgs nixpkgset;
            };
            nullArgs = system: make.specialArgs null system;
            config = name: system: nixosSystem rec {
                specialArgs = make.specialArgs name system;
                modules = with inputs; let
                    j-list = specialArgs.lib.j.functions.list;
                in flatten [
                    "./host/${name}"
                    home-manager.nixosModules.home-manager
                    impermanence.nixosModules.impermanence
                    (j-list { dir = ./modules; })
                    (j-list { dir = ./secrets; })
                ];
            };
            nixosConfiguration = system: { packages.nixosConfigurations = listToAttrs (map
                (name: nameValuePair name (make.config name system))
                (attrNames (filterAttrs (n: v: v == "directory") (readDir ./hosts)))
            ); };
            all = system: (make.nullArgs system) // (make.nixosConfiguration system);
        };
    in (eachSystem allSystems make.all) // { inherit make channel; };
}
