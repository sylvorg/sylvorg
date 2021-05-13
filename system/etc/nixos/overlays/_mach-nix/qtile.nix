{
  sources ? ((import (
        let
            lock = builtins.fromJSON (builtins.readFile ../flake.lock);
        in fetchTarball {
            url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
            sha256 = lock.nodes.flake-compat.locked.narHash;
        }
    ) { src =  ./. + "/.."; }).defaultNix),
  pkgs ? (import sources.nixpkgs {}),
  mach-nix ? (import sources.mach-nix { inherit pkgs; python = "python39"; }),
  ...
}:

with builtins; let

    # build a mach-nix python env
    machnixPy = mach-nix.mkPython {
        packagesExtra = [
            # "https://github.com/psf/requests/tarball/v2.22.0"
            (mach-nix.buildPythonPackage { src = sources.nanite; })      
            "https://files.pythonhosted.org/packages/bc/ab/c49f97516f78c2b0cacb4f45873abc4ca9872942a9c4c19ded8052c8edda/python-wifi-0.6.1.tar.bz2"
        ];
        requirements = ''
        cairocffi
        iwlib
        '';
        _.iwlib.buildInputs.add = [ pkgs.wirelesstools ];
        providers._defaults = "conda-forge,conda,wheel,sdist,nixpkgs";
        providers.cairocffi = "wheel,sdist";
    };

    pythonPath =
        oa.pythonPath
        ++ machnixPy.selectPkgs pythonPackages
        ++ (with pythonPackages; [
            # From https://github.com/NixOS/nixpkgs/issues/45038
            dateutil
            dbus-python
            keyring
            mpd2
            psutil
            pyxdg
            pygobject3
            nixpkgs
        ]);


    newPkgs = machnixPy.nixpkgs;
    pythonPackages = newPkgs.python39.pkgs;
    qtile_py_39 = newPkgs.qtile.override {
        python37Packages = newPkgs.python39Packages;
    };
    qtile_with_pkgs = qtile_py_39.overrideAttrs (oa: {
        version = "master";
        src = sources.qtile;
        inherit pythonPath;
    });
    finalOverlay = self: super: { qtile = qtile_with_pkgs; };
in finalOverlay
