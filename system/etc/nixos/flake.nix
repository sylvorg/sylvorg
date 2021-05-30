{
    description = "Wheee!";

    inputs = rec {
        home-manager-flake = {
            url = "github:nix-community/home-manager/master";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        
        impermanence-flake = {
            url = "github:nix-community/impermanence/master";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        
        agenix = {
            url = "github:ryantm/agenix/master";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        
        flake-utils = {
            url = "github:numtide/flake-utils/master";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        
        hlissner = {
            url = "github:hlissner/dotfiles/master";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        
        flake-compat = {
            url = "github:edolstra/flake-compat";
            flake = false;
        };
        
        nixpkgs.url = "github:NixOS/nixpkgs/master";
        aleclearmind = {
            url = "github:aleclearmind/nested-tmux";
            flake = false;
        };
        oh-my-tmux = {
            url = "github:gpakosz/.tmux";
            flake = false;
        };
        powerline = {
            url = "github:powerline/powerline";
            flake = false;
        };
        tpm = {
            url = "github:tmux-plugins/tpm";
            flake = false;
        };
        doom-emacs = {
            url = "github:hlissner/doom-emacs";
            flake = false;
        };
        spacevim = {
            url = "github:spacevim/spacevim";
            flake = false;
        };
        bashCompletions = {
            url = "github:scop/bash-completion";
            flake = false;
        };
        grml = {
            url = "github:grml/grml-etc-core";
            flake = false;
        };
        kittyThemes = {
            url = "github:dexpota/kitty-themes";
            flake = false;
        };
        xeroFigletFonts = {
            url = "github:xero/figlet-fonts";
            flake = false;
        };
        extraContainer = {
            url = "github:erikarvstedt/extra-container";
            flake = false;
        };
        nixos-surface = {
            url = "github:anthe/nixos-surface";
            flake = false;
        };
        xanmodV5104Cacule = {
            url = "github:xanmod/linux/5.10.4-xanmod1-cacule";
            flake = false;
        };
        xanmodV5914Cacule = {
            url = "github:xanmod/linux/5.9.14-xanmod1-cacule";
            flake = false;
        };
    };

    outputs = inputs@{ self, nixpkgs, flake-utils, flake-compat, ... }: with builtins; with nixpkgs.lib; with flake-utils.lib; let

        mkOverlay = import ./overlays;

        prepkgs = import nixpkgs {
            overlays = mkOverlay {};
            config = {
                system = currentSystem;
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

        sources = inputs // prepkgs.j.sources;
        inherit (sources) nix;
        lib = nixpkgs.lib.extend (final: prev: {
            j = import ./lib {
                inherit sources;
                pkgs = prepkgs;
                lib = final;
            };
            h = sources.hlissner.lib;
        });

        inherit (lib) j;
        inherit (lib.j) attrs;
        fas = j.forAllSystems;
        make = {
            overlay = mkOverlay;
            pkgs = { stc, ... }: import prepkgs.j.nixpkgset.${stc.channel} {
                overlays = j.get { inherit stc; set = all.overlays; };
                config =  j.get { inherit stc; set = all.config; };
            };
            specialArgs = { stc, ... }: let
                configBase = { inherit stc; ignoredAttrs = [ "host" ];};
                config =  j.get (configBase // { set = all.config; });
                overlays =  j.get (configBase // { set = all.overlays; });
            in stc // {
                inherit sources inputs make all stc overlays lib;
                config' = config;
                hostName =  j.get { inherit stc; set = all.hostName; };
                stdenv =  j.get (configBase // { set = all.stdenv; });
                nixpkgs =  j.get (configBase // { set = all.nixpkgs; });
                nixpkgset = {
                    inherit (stc) system;
                    inherit overlays config;
                };
            };
            modules = { stc, ... }: [
                (with stc; [
                    (./. + "/configs/${host}")
                    (if (type == "def") then {} else (./. + "/devices/${type}"))
                    (let path = ./. + "/platforms/${system}"; in
                        if (pathExists path) then path else {})
                ])
                (with sources; [
                    home-manager-flake.nixosModules.home-manager
                    agenix.nixosModules.age
                    impermanence-flake.nixosModules.impermanence
                ])
            ];
            nixosConfiguration = { stc, ... }: lib.nixosSystem {
                inherit (stc) system;
                pkgs = let configBase = { inherit stc; ignoredAttrs = [ "host" ];}; in j.get (configBase // { set = all.pkgs; });
                specialArgs = make.specialArgs { inherit stc; };
                modules = flatten [
                    (j.imprelib.list { dir = ./modules; })
                    (make.modules { inherit stc; })
                ];
            };
            nixosModule = { stc, ... }: nmports@{ config, ... }: { imports = flatten [
                (make.modules nmports { inherit stc; })
            ];};
        };
        all' = {
            inherit sources make;
            type = attrs.types;
            device = attrs.devices;
            nixpkgs = prepkgs.j.nixpkgset;
            channel = prepkgs.j.channels;
            host = attrs.hosts;
            # system = allSystems;
            system = [ "aarch64-linux" "x86_64-linux" ];
            # system = defaultSystems;
        } // (genAttrs (attrNames attrs.integer-defaults) (attr: range 0 1));
        all = let
            sc = {
                all = all';
                inheritance.pkgs = import nixpkgs {};
            };
        in {
            config = fas (recursiveUpdate sc {
                func = j.config;
                inheritance.stdenvs = all.stdenv;
            });
            hostName = fas {
                all = all';
                func = j.hostName;
                attrList = attrs.stc ++ (toList "host");
            };
            overlays = fas {
                all = all';
                func = make.overlay;
            };
            pkgs = fas {
                all = all';
                func = make.pkgs;
            };
            stdenv = fas (recursiveUpdate sc { func = j.stdenv; });
            hmConfigs = fas {
                all = all';
                func = j.hostName;
                inherit (all.vars) extraListSets;
            };
            vars = {
                extraListSets = {
                    user = attrs.allUsers;
                } // (genAttrs attrs.home-manager-integer-defaults (attr: range 0 1));
            };
        } // all';

        overlays =  j.get { stc = attrs.default-stc; set = all.overlays; };

    in with lib; with j; {

        inherit overlays lib;

        legacyPackages = all;

        nixosConfigurations = (forAllSystems' { inherit all; func = make.nixosConfiguration; }) // {
            tiny = let
                stc = lib.j.attrs.default-stc // {
                    device = "";
                    host = "";
                    type = "";
                    zfs = null;
                };
            in if (with stc;
                device == "" || host == "" || type == "" || zfs == null
            ) then (
                abort "Sorry! The device, host, type, and zfs status must be set!"
            ) else lib.nixosSystem {
                inherit (stc) system;
                pkgs = let configBase = { inherit stc; ignoredAttrs = [ "host" ];}; in j.get (configBase // { set = all.pkgs; });
                specialArgs = make.specialArgs { inherit stc; };
                modules = flatten [
                    (make.modules { inherit stc; })
                    ({ ... }: { imports = [
                        ./modules/networking.nix
                        ./modules/boot.nix
                        ./modules/etc.nix
                        ./modules/global.nix
                        ./modules/users.nix
                        ./modules/filesystems.nix
                        ./modules/nix.nix
                        ./modules/persistence.nix
                        ./modules/variables.nix
                        ./modules/zfs.nix
                    ];})
                ];
            };
        };

        # From: https://nixos.wiki/wiki/Flakes#Getting_Instant_System_Flakes_Repl
        nix.nixPath = let path = toString ./.; in [ "repl=${path}/repl.nix" "nixpkgs=${sources.nixpkgs}" ];

    } // (eachSystem all.system (system: {  }));

}
