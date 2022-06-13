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
        nixos-master.url = github:NixOS/nixpkgs/master;
        nixpkgs.follows = "nixos-22-05";

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
        channel = "nixos-22-05";
        make = {
            base = {
                lib = system: nixpkgs.lib.extend (final: prev: {
                    j = import ./lib.nix {
                        inherit inputs system;
                        pkgs = import nixpkgs { inherit system; };
                        lib = final;
                    };
                    inherit (inputs.home-manager.lib) hm;
                });
                nixpkgset = {
                    default = system: lib: { inherit system; config = lib.j.attrs.configs.nixpkgs; };
                    overlayed = overlays: system: lib: { inherit overlays system; config = lib.j.attrs.configs.nixpkgs; };
                };
                overlays = system: lib: import ./overlays.nix {
                    inherit lib nixpkgs inputs channel;
                    pkgs = mapAttrs (n: v: import v (make.base.nixpkgset.default system lib)) (filterAttrs (n: v: (hasPrefix "nixos-" n) || (hasPrefix "release-" n)) inputs);
                };
                pkgs = nixpkgset: import nixpkgs nixpkgset;
                specialArgs = system: rec {
                    inherit inputs system;
                    lib = make.nameless.lib system;
                    nixpkgset = {
                        default = make.nameless.nixpkgset.default system lib;
                        overlayed = make.nameless.nixpkgset.overlayed overlays system lib;
                    };
                    overlays = make.nameless.overlays system lib;
                    pkgs = make.nameless.pkgs nixpkgset.overlayed;
                };
                app = drv: { type = "app"; program = "${drv}${drv.passthru.exePath or "/bin/${drv.meta.mainprogram or drv.pname or drv.name}"}"; };
            };
            nameless = recursiveUpdate make.base {
                outputs = system: rec {
                    inherit make;
                    specialArgs = make.nameless.specialArgs system;
                    legacyPackages = specialArgs.pkgs;
                    defaultPackage = flattenTree { settings = legacyPackages.settings; };
                    # packages = flattenTree (filterAttrs (n: v: all (b: b == true) [
                    #     (! elem n [ "prometheus-dmarc-exporter" ])
                    #     (tryEval v).success
                    #     ((isDerivation v) && (v ? meta) && (v.meta ? broken))
                    # ]) legacyPackages);
                    # apps = mapAttrs (n: v: make.nameless.app v) packages;
                    apps = mapAttrs (n: v: make.nameless.app v) legacyPackages;
                    defaultApp = make.nameless.app legacyPackages.settings;
                };
            };
            named = recursiveUpdate make.base {
                specialArgs = name: system: recursiveUpdate (make.named.specialArgs system) { host = name; };
                config = name: system: nixosSystem rec {
                    specialArgs = make.named.specialArgs name system;
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

                nixosConfiguration = system: { packages.nixosConfigurations = genAttrs (dirCon.dirs ./hosts) (name: make.named.config name system); };

                # nixosConfiguration = system: { packages.nixosConfigurations = listToAttrs (map
                #     (name: nameValuePair name (make.named.config name system))
                #     (dirCon.dirs ./hosts)
                # ); };
            };
            both = system: (make.named.nixosConfiguration system) // (make.nameless.outputs system);
        };

    in (let _ = eachSystem allSystems make.both; in trace _.packages.x86_64-linux _);
}
