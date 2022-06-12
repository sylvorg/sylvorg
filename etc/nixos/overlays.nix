args@{ lib, nixpkgs, inputs, pkgs, channel }: with builtins; with lib;
let
    pv2 = "python2${j.attrs.versions.python.two}";
    updatePython2 = prev: attrs: { "${pv2}" = prev.${pv2} // { pkgs = prev.${pv2}.pkgs // attrs; }; };
    updatePython2Packages = prev: dir: updatePython2 prev (j.import.set { call = final.${pv2}.pkgs; inherit dir; ignores = j.dirCon.dirs dir; });
    pv3 = "python3${j.attrs.versions.python.three}";
    updatePython3 = prev: attrs: { "${pv3}" = prev.${pv3} // { pkgs = prev.${pv3}.pkgs // attrs; }; };
    updatePython3Packages = prev: dir: updatePython3 prev (j.import.set { call = final.${pv3}.pkgs; inherit dir; ignores = j.dirCon.dirs dir; });
in flatten [
(final: prev: { j = { inherit pkgs; };})
(final: prev: rec {
    python2 = final.${pv2};
    python3 = final.${pv3};
    python = final.python3;
})
(final: prev: updatePython3 prev { rich = prev.pythonPackages.rich.overridePythonAttrs (old: {
    version = "12.0.0";
    src = final.fetchFromGitHub {
        owner = "syvlorg";
        repo = old.pname;
        rev = "a6c20ce10adc7b8cfacfd74e0b025e8c2c8c19eb";
        sha256 = "1ld3ihvssfk56240wignmd6hv7gynid5wmcynl58ng8sbfywm3ly";
    };
    propagatedBuildInputs = (with final.pythonPackages; [ hy ]) ++ old.propagatedBuildInputs;
    meta = {
        description = "Render rich text, tables, progress bars, syntax highlighting, markdown and more to the terminal";
        homepage = "https://github.com/syvlorg/rich";
        license = lib.licenses.mit;
    };
}); })
(final: prev: { xonsh = prev.xonsh.overridePythonAttrs (old: { propagatedBuildInputs = (with final.pythonPackages; [ 
    bakery
    xontrib-sh
    xontrib-readable-traceback
    xontrib-pipeliner
    xonsh-autoxsh
    xonsh-direnv
]) ++ old.propagatedBuildInputs; }); })
(final: prev: { nur = import inputs.nur { nurpkgs = nixpkgs; pkgs = prev; }; })
inputs.emacs.overlay
(final: prev: let dir = ./callPackages; in j.import.set { call = true; inherit dir; ignores = j.dirCon.dirs dir; })
(final: prev: updatePython2Packages prev ./callPackages/python2)
(final: prev: updatePython3Packages prev ./callPackages/python3)
(final: prev: let dir = ./overlays; in j.import.set { inherit dir; ignores = j.dirCon.dirs dir; })
(let pkgsets = {
    # nixos-unstable = [ "gnome-tour" ];
    # nixos-unstable = "gnome-tour";
    # nixos-unstable = { python3 = "python310"; };
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
            pkg2IsString = isString pkg2Pre;
            self = (pkgchannel == channel) || (pkgchannel == "self");
            pkgFunc = pkg: { "${pkg}" = if self then prev.${pkg} else final.j.pkgs.${pkgchannel}.${pkg1}.${pkg}; };
            pkg2 = if pkg2IsString then (pkgFunc pkg2Pre) else (genAttrs pkg2Pre pkgFunc);
        in final: prev: { "${pkg1}" = pkg2; }
    ) pkglist
) pkgsets)
]
