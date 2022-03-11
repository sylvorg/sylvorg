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
    # nixos-unstable = [ "networkmanager" "gnome-control-center" "bash" "bootstrap-tools" { gcc10 = "gcc11"; } ];
    nixos-unstable = [ ];
};
in mapAttrsToList (
    pkgchannel: pkglist': let
        pkglist = if (isString pkglist') then [ pkglist' ] else pkglist';
    in map (
        pkg': let
            pkgIsAttrs = isAttrs pkg;
            pkg1 = if pkgIsAttrs then (last (attrNames pkg)) else pkg;
            pkg2 = if pkgIsAttrs then (last (attrValues pkg)) else pkg;
            self = (pkgchannel == channel) || (pkgchannel == "self");
        in (final: prev: {
            "${pkg1}" = if self then prev.${pkg2} else final.j.pkgs.${pkgchannel}.${pkg2};
        })
    ) pkglist
) pkgsets)
]
