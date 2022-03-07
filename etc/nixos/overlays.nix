args@{ lib, nixpkgs, inputs, pkgs }: with builtins; with lib;
let
in flatten [
(final: prev: { j = { inherit pkgs; };})
(final: prev: { nur = import inputs.nur { nurpkgs = nixpkgs; pkgs = prev; }; })
inputs.emacs.overlay
inputs.mozilla.overlays
(map (file:
    [(final: prev: {
        "${j.functions.name { inherit file; }}" = import file args;
    })]
) (j.functions.list { dir = ./overlays; ignores = [ "nix" ]; }))
(let pkgsets = {
    unstable = [  ];
};
in mapAttrsToList (
    pkgchannel: pkglist: map (
        pkg: [(final: prev: {
            "${pkg}" = if (pkgchannel == channel) then prev.${pkg} else final.j.pkgs.${pkgchannel}.${pkg};
        })]
    ) pkglist
) pkgsets)
]
