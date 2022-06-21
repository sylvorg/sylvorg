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
        patch = {
            nixpkgs = let
                patches' = [ ./patches/nixpkgs/bcachefs-module.patch ];
            in {
                default = src: config: nixpkgs // ((import src config).applyPatches {
                    name = "defaultPatches";
                    inherit src;
                    patches = patches';
                });
                extras = src: config: patches: nixpkgs // ((import src config).applyPatches { name = "extraPatches"; inherit src patches; });
                both = src: config: patches: nixpkgs // ((import src config).applyPatches {
                    name = "bothPatches";
                    inherit src;
                    patches = patches' ++ patches;
                });
            };
            pkgs = {
                default = src: config: import (patch.nixpkgs.default src config) config;
                extras = src: config: patches: import (patch.nixpkgs.extras src config patches) config;
            };
        };
        make = {
            base = {
                nixpkgset = {
                    base = system: { inherit system; };
                    default = system: lib: (make.base.nixpkgset.base system) // { config = lib.j.attrs.configs.nixpkgs; };
                    overlayed = overlays: system: lib: (make.base.nixpkgset.default system lib) // { inherit overlays; };
                };
                nixpkgs = {
                    base = system: patch.nixpkgs.default nixpkgs (make.base.nixpkgset.base system);
                    default = system: lib: patch.nixpkgs.default nixpkgs (make.base.nixpkgset.default system lib);
                    overlayed = overlays: system: lib: patch.nixpkgs.default nixpkgs (make.base.nixpkgset.overlayed overlays system lib);
                };
                pkgs = {
                    base = system: patch.pkgs.default nixpkgs (make.base.nixpkgset.base system);
                    default = system: lib: patch.pkgs.default nixpkgs (make.base.nixpkgset.default system lib);
                    overlayed = overlays: system: lib: patch.pkgs.default nixpkgs (make.base.nixpkgset.overlayed overlays system lib);
                };
                lib = system: (make.base.nixpkgs.base system).lib.extend (final: prev: {
                    j = import ./lib.nix {
                        inherit inputs system;
                        inherit (make.base.pkgs.default system final) hello;
                        lib = final;
                        extras = { inherit patch; };
                    };
                    inherit (inputs.home-manager.lib) hm;
                });
                overlays = system: lib: import ./overlays.nix {
                    inherit lib inputs channel;
                    nixpkgs = make.base.nixpkgs system;
                    pkgs = mapAttrs (n: v: patch.pkgs.default v (make.base.nixpkgset.default system lib))
                                    (filterAttrs (n: v: (hasPrefix "nixos-" n) || (hasPrefix "release-" n)) inputs);
                };
                specialArgs = system: rec {
                    inherit inputs;
                    lib = make.base.lib system;
                    nixpkgset = {
                        base = make.base.nixpkgset.base system;
                        default = make.base.nixpkgset.default system lib;
                        overlayed = make.base.nixpkgset.overlayed overlays system lib;
                    };
                    nixpkgs = {
                        base = make.base.nixpkgs.base system;
                        default = make.base.nixpkgs.default system lib;
                        overlayed = make.base.nixpkgs.overlayed overlays system lib;
                    };
                    pkgs = {
                        base = make.base.pkgs.base system;
                        default = make.base.pkgs.default system lib;
                        overlayed = make.base.pkgs.overlayed overlays system lib;
                    };
                    overlays = make.base.overlays system lib;
                };
                app = drv: { type = "app"; program = "${drv}${drv.passthru.exePath or "/bin/${drv.meta.mainprogram or drv.pname or drv.name}"}"; };
            };
            nameless = recursiveUpdate make.base {
                outputs = system: rec {
                    inherit system;
                    specialArgs = make.nameless.specialArgs system;
                    inherit (specialArgs) nixpkgset nixpkgs pkgs overlays lib;
                    overlay = final: prev: { settings = final.callPackage ./callPackages/settings.nix {}; };
                    defaultOverlay = overlay;
                    legacyPackages = pkgs.overlayed;
                    apps = mapAttrs (n: v: make.nameless.app v) pkgs.overlayed;
                    app = apps.default;
                    defaultApp = app;
                    package = packages.settings;
                    defaultPackage = package;
                };
            };
            named = recursiveUpdate make.base {
                specialArgs = name: system: (make.nameless.specialArgs system) // { host = name; };
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

    in (eachSystem allSystems make.both) // { inherit make; };
}
