args@{ lib, nixpkgs, inputs, pkgs, channel }: with builtins; with lib;
let
in flatten [
(final: prev: { j = { inherit pkgs; };})
(final: prev: { nur = import inputs.nur { nurpkgs = nixpkgs; pkgs = prev; }; })
inputs.emacs.overlay
(map (file:
    (final: prev: {
        "${j.name { inherit file; }}" = import file args;
    })
) (j.import.list { dir = ./overlays; }))
(let pkgsets = {
    # nixos-unstable = [ "gnome-tour" ];
    nixos-unstable = "gnome-tour";
};
in mapAttrsToList (
    pkgchannel: pkglist': let
        pkglist = if (isString pkglist') then [ pkglist' ] else pkglist';
    in map (
        pkg': let
            pkgIsAttrs = isAttrs pkg';
            pkg1 = if pkgIsAttrs then (last (attrNames pkg')) else pkg';
            pkg2 = if pkgIsAttrs then (last (attrValues pkg')) else pkg';
            self = (pkgchannel == channel) || (pkgchannel == "self");
        in final: prev: { "${pkg1}" = if self then prev.${pkg2} else final.j.pkgs.${pkgchannel}.${pkg2}; }
    ) pkglist
) pkgsets)
(let pkgsets = {
    # nixos-unstable = [ { python310Packages = "mypy"; } { python310Packages = [ "mypy" ]; } ];
    # nixos-unstable = { python310Packages = "mypy"; };
    # nixos-unstable = { python310Packages = [ "mypy" ]; };
    # nixos-22-05 = { python310Packages = "mypy"; };
};
in mapAttrsToList (
    pkgchannel: pkglist': let
        pkglist = if (isAttrs pkglist') then [ pkglist' ] else pkglist';
    in map (
        pkg': let
            pkg1 = last (attrNames pkg');
            pkg2Pre = last (attrValues pkg');
            pkg2IsString = isStr pkg2Pre;
            self = (pkgchannel == channel) || (pkgchannel == "self");
            pkgFunc = pkg: { "${pkg}" = if self then prev.${pkg} else final.j.pkgs.${pkgchannel}.${pkg1}.${pkg}; };
            pkg2 = if pkg2IsString then (pkgFunc pkg2Pre) else (genAttrs pkg2Pre pkgFunc);
        in final: prev: { "${pkg1}" = pkg2; }
    ) pkglist
) pkgsets)
]
