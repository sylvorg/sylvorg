with builtins; {
    stc ? ({ system = currentSystem; channel = "pkgs"; }),
    ...
} : with stc; let

    sources = (import (
        let
            lock = builtins.fromJSON (builtins.readFile ./flake.lock);
        in fetchTarball {
            url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
            sha256 = lock.nodes.flake-compat.locked.narHash;
        }
    ) { src =  ./.; }).defaultNix;

    nprefix = "nixpkgs";
    prefix = "pkgs";
    inherit (sources) nixpkgs;
    pkgs = import nixpkgs {
        inherit (stc) system;
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
    lib = nixpkgs.lib.extend (final: prev: {
        j = import ../lib {
            inherit sources pkgs;
            lib = final;
        };
    });
in with lib; flatten [
    (
        let
            sc = { inherit pkgs stc; };
            stdenv = j.stdenv sc;
            config = j.config (sc // { inherit stdenv; });
        in flatten [
            [(final: prev: { j = rec {
                inherit config stdenv sources;
                inherit (sources) nixpkgs;
                nixpkgset = 
                    let
                        withNPrefix = filterAttrs (
                            n: v: (hasPrefix nprefix n) || (n == nprefix)
                        ) sources;
                        withoutNPrefix = mapAttrs' (name: v: nameValuePair (
                            if (name == nprefix) then prefix else (
                                replaceStrings ["${nprefix}-"] [""] name
                            )
                        ) v) withNPrefix;
                    in removeAttrs withoutNPrefix (flatten [
                        (filter (name: !elem name [
                            "pkgs"
                            "unstable"
                        ]) (attrNames withoutNPrefix))
                        [  ]
                    ]);
                pkgset = (
                    mapAttrs (n: v: import v { inherit config; }) nixpkgset
                ) // { "${channel}" = final; };
                channels = attrNames nixpkgset;
            };})]
            # [(
            #     final: prev: {
            #         fetchurl = prev.fetchurl.overrideAttrs (attrs: { patches = attrs.patches ++ [ ../patches/fetchurl.patch ];}); }
            # )]
            # [(
            #     final: prev: {
            #         fetchpypi = prev.fetchpypi.overrideAttrs (attrs: { patches = attrs.patches ++ [ ../patches/fetchpypi.patch ];}); }
            # )]
            [
                # (import sources.emacs)
                # (final: prev: {
                #     nur = import sources.nur {
                #         nurpkgs = prev;
                #         pkgs = prev;
                #     };
                # })
                (import ("${sources.wip-pinebook-pro}/overlay.nix"))
                sources.emacs.overlay
                sources.nur.overlay
            ]
            [
                (final: prev: { nix = sources.nix.packages.${system}.nix; })
                # (final: prev: { nix = (import sources.nix).packages.${system}.nix; })
                (final: prev: { niv = (import sources.niv {}).niv; })
                (final: prev: { emacs-nox = final.emacsGit-nox; })
            ]
            [
                (final: prev: {
                    systemd = prev.systemd.overrideAttrs (old: { withHomed = true; });
                })
                (final: prev: {
                    kata-containers = prev.kata-containers or (
                        prev.callPackage ./_kataContainers.nix {}
                    );
                })
            ]
            (
                let
                    dir = sources.mozilla;
                    mozilla = final: prev: listToAttrs (map (file: nameValuePair
                        (removeSuffix "-overlay" file)
                        (import "${dir}/${file}.nix" final prev)
                    ) (filter (file: hasSuffix "-overlay" file) (j.imprelib.listNames { inherit dir; })));
                in [
                    (final: prev: { firefox = final.mozilla.firefox.latest.firefox-bin; })
                    (final: prev: { firefox-unbuilt = prev.firefox; })
                    (final: prev: { mozilla = mozilla final prev; })
                ]
            )
        
            # TODO
            # (map (kernel': let
            #     kernel = "linuxPackages_${kernel'}";
            # in [( final: prev: { "${kernel}" = prev."${kernel}".extend (self:
            #     const (super: { kernel = super.kernel.overrideDerivation (drv: {
            #         nativeBuildInputs = drv.nativeBuildInputs ++ [ pkgs.zstd ];
            #     });})
            # );})]) [ "lqx" "testing_bcachefs" "zen" ])
        
            # TODO
            # (let base.mach-nix = {
            #     inherit pkgs sources lib;
            #     mach-nix = import sources.mach-nix { inherit pkgs; python = "python39"; };
            # }; in map (overlay: import overlay base.mach-nix) (j.imprelib.list { dir = ./_mach-nix; }))
        
            (flatten (map (file:
                [(final: prev: {
                    "${j.imprelib.name { inherit file; }}" = import file {
                        inherit sources pkgs lib;
                    };
                })]
            ) (j.imprelib.list { dir = ./.; ignores = [ "nix" ]; })))
        
            # TODO
            # (
            #     map (ver: let
            #             inherit ver;
            #             _ = makeExtensible { "linuxPackages_xanmod_v${ver}_cacule" = prev.recurseIntoAttrs (
            #                 prev.linuxPackagesFor (
            #                     prev.callPackage (
            #                         args@{ fetchFromGitHub, buildLinux, ... }: buildLinux (args // rec {
            #                             version = "${ver}-xanmod1";
            #                             modDirVersion = version;
            #                             src = sources."xanmodV${replace ["."] [""] ver}Cacule" // { extraPostFetch = '' rm $out/.config ''; };
            #                             kernelPatches = [];
            #                             # postConfigure = '' make ARCH=x86_64 mrproper '';
            #                             extraConfig = ''
            #                             #     USER_NS_UNPRIVILEGED y
            #                             #     FUNCTION_TRACER n
            #                             #     GRAPH_TRACER n
            #                             #     NUMA n
            #                                 RD_ZSTD y
            #                                 KERNEL_ZSTD y
            #                                 KERNEL_XZ n
            #                             '';
            #                             extraMeta.branch = "${ver}-xanmod1";
            #                         } // (args.argsOverride or {}))
            #                     ) {}
            #                 )
            #             );};
            #         in [( final: prev: { "linuxPackages_xanmod_v${ver}_cacule" = _.extend (self:
            #             const (super: { kernel = super.kernel.overrideDerivation (drv: {
            #                 nativeBuildInputs = drv.nativeBuildInputs ++ [ pkgs.zstd ];
            #             });})
            #     );})]) [ "5.9.14" "5.10.4" ]
            # )
        ]
    )
    (
        let pkgsets' = { unstable = [ "git" "go" "webkitgtk" ]; };
        in flatten (mapAttrsToList (
            channel': pkglist: map (
                pkg: [(final: prev: {
                    "${pkg}" = if (channel' == channel) then prev.${pkg} else final.j.pkgset.${channel'}.${pkg};
                })]
            ) pkglist
        ) pkgsets')
    )
]
