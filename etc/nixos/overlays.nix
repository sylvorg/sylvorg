lib: nixpkgs: pkgs: channel: with builtins; with lib;
let
channels = (map (d: "nixos-" + d) (
    "unstable"
    "unstable-small"
    "21.11"
    "21.11-small"
)) + (map (d: "release-" + d) (
    "21.11"
)) + (
    "master"
)
othernixpkgs = listToAttrs (map (ref: nameValuePair
    ref
    (fetchGit { url = "https://github.com/nixos/nixpkgs"; inherit ref; })
) channels)
otherpkgs = mapAttrs (n: v: import v j.attrs.configs.nixpkgs) othernixpkgs;
in flatten [
[(final: prev: { j = {
    nixpkgs = othernixpkgs;
    pkgs = otherpkgs;
    inherit channels;
};})]
[( final: prev: { nix = (fetchGit { url = "https://github.com/nix-community/nur"; }).defaultNix; })]
[( final: prev: { nur = import (fetchGit { url = "https://github.com/nix-community/nur"; }) { nurpkgs = nixpkgs; pkgs = prev; }; })]
[
    (import (fetchGit { url = "https://github.com/nix-community/emacs-overlay"; }))
    (final: prev: { emacs-nox = final.emacsGit-nox; })
    (final: prev: { emacs = final.emacsGit; })
]
[( final: prev: { systemd = prev.systemd.overrideAttrs (old: { withHomed = true; }); })]
[
    (final: prev: {
        extra-container = let
            pkgSrc = fetchGit { url = "https://github.com/erikarvstedt/extra-container"; };
        in pkgs.callPackage pkgSrc { inherit pkgSrc; };
    })
]
(import "${fetchGit { url = "https://github.com/mozilla/nixpkgs-mozilla"; }}/overlays.nix")
(flatten (map (file:
    [(final: prev: {
        "${j.functions.name { inherit file; }}" = import file {
            inherit sources pkgs lib;
        };
    })]
) (j.functions.list { dir = ./overlays; ignores = [ "nix" ]; })))
(let pkgsets = {
    unstable = [  ];
};
in flatten (mapAttrsToList (
    pkgchannel: pkglist: map (
        pkg: [(final: prev: {
            "${pkg}" = if (pkgchannel == channel) then prev.${pkg} else final.j.pkgs.${pkgchannel}.${pkg};
        })]
    ) pkglist
) pkgsets))
[(final: prev: { guix = final.callPackage "${fetchGit { url = "https://github.com/${j.attrs.users.primary}/nixpkgs"; ref = "guix"; }}/pkgs/development/guix/guix.nix" {  }; })]
]
