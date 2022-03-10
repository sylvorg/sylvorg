args@{ lib, nixpkgs, inputs, pkgs, channel }: with builtins; with lib;
let
in flatten [
(final: prev: { j = { inherit pkgs; };})
(final: prev: { nur = import inputs.nur { nurpkgs = nixpkgs; pkgs = prev; }; })
inputs.emacs.overlay
(map (file:
    (final: prev: {
        "${j.functions.name { inherit file; }}" = import file args;
    })
) (j.functions.list { dir = ./overlays; }))
(let pkgsets = {
    # nixos-unstable = [ "networkmanager" "gnome-control-center" "bash" "bootstrap-tools" ];
    nixos-unstable = [ ];
};
in mapAttrsToList (
    pkgchannel: pkglist: map (
        pkg: (final: prev: {
            "${pkg}" = if (pkgchannel == channel) then prev.${pkg} else final.j.pkgs.${pkgchannel}.${pkg};
        })
    ) pkglist
) pkgsets)
]
