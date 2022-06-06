{
    nixConfig = {
        # Adapted From: https://github.com/divnix/digga/blob/main/examples/devos/flake.nix#L4
        # extra-substituters = "https://cache.nixos.org/ https://nix-community.cachix.org/";
        trusted-substituters = "https://cache.nixos.org/";
        # extra-trusted-public-keys = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
        trusted-public-keys = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
        # keep-derivations = true;
        # keep-outputs = true;
        extra-experimental-features = "nix-command flakes";
        # allow-unsafe-native-code-during-evaluation = true;
        # min-free = 262144000;
        # max-free = 1073741824;
    };

    inputs = rec {
        nixos-unstable.url = github:NixOS/nixpkgs/nixos-unstable;
        nixos-unstable-small.url = github:NixOS/nixpkgs/nixos-unstable-small;
        nixos-22-05.url = github:NixOS/nixpkgs/nixos-22.05;
        nixos-22-05-small.url = github:NixOS/nixpkgs/nixos-22.05-small;
        nixos-21-11.url = github:NixOS/nixpkgs/nixos-21.11;
        nixos-21-11-small.url = github:NixOS/nixpkgs/nixos-21.11-small;
        master.url = github:NixOS/nixpkgs/master;
        j.url = github:shadowrylander/nixpkgs/j;
        nixpkgs.follows = "j";

        hardware.url = github:nixos/nixos-hardware;

        pinebook-pro = {
            url = github:samueldr/wip-pinebook-pro;
            flake = false;
        };

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
            pre-pkgs = system: import nixpkgs { inherit system; };
            lib = system: nixpkgs.lib.extend (final: prev: {
                j = import ./lib.nix {
                    inherit inputs system;
                    pkgs = make.pre-pkgs system;
                    lib = final;
                };
                inherit (inputs.home-manager.lib) hm;
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
                lib = make.lib system;
                pre-nixpkgset = make.pre-nixpkgset system lib;
                overlays = make.overlays system lib;
                nixpkgset = make.nixpkgset overlays system lib;
                pkgs = make.pkgs nixpkgset;
            };
            config = name: system: nixosSystem rec {
                specialArgs = make.specialArgs name system;
                modules = with inputs; let
                    j-list = specialArgs.lib.j.import.list;
                in flatten [
                    "${toString ./.}/hosts/${name}"
                    home-manager.nixosModules.home-manager
                    impermanence.nixosModules.impermanence
                    (j-list { dir = ./modules; })
                    (j-list { dir = ./secrets; })
                ];
            };

            nixosConfiguration = system: { packages.nixosConfigurations = genAttrs (attrNames (filterAttrs (n: v: v == "directory") (readDir ./hosts))) (name: make.config name system); };

            # nixosConfiguration = system: { packages.nixosConfigurations = listToAttrs (map
            #     (name: nameValuePair name (make.config name system))
            #     (attrNames (filterAttrs (n: v: v == "directory") (readDir ./hosts)))
            # ); };
        };
    in recursiveUpdate (eachSystem allSystems make.nixosConfiguration) { inherit make channel; };
}
