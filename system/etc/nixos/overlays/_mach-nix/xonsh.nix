{
  sources ? (builtins.getFlake "${./.}/.."),
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
        # add requirements here
        # jedi
        borgmatic
        dephell
        ply
        prompt_toolkit
        pygments
        pipx
        yubico-client
        ansible

        # add xontribs here
        # xonsh-direnv
        # xonsh-docker-tabcomplete
        # xonsh-vox-tabcomplete
        # xontrib-prompt-bar
        # xontrib-ssh-agent
        xonsh-autoxsh
        xontrib-autojump
        xontrib-fzf-widgets
        xontrib-kitty
        xontrib-pipeliner
        xontrib-powerline2
        xontrib-prompt-vi-mode
        xontrib-readable-traceback
        xontrib-schedule
        xontrib-z
        '';
        providers._defaults = "conda-forge,conda,wheel,sdist,nixpkgs";
    };

    pythonPath =
        (oa.pythonPath or [])
        ++ machnixPy.selectPkgs pythonPackages
        ++ (with pythonPackages; [
            ply
            prompt_toolkit
            pygments
            pipx
            yubico-client
            nixpkgs
        ]);


    newPkgs = machnixPy.nixpkgs;
    pythonPackages = newPkgs.python39.pkgs;
    xonsh_py_39 = newPkgs.xonsh.override {
        python3Packages = newPkgs.python39Packages;
    };
    xonsh_with_pkgs = xonsh_py_39.overrideAttrs (oa: {
        version = "master";
        src = sources.xonsh;
        inherit pythonPath;
    });
    finalOverlay = self: super: { xonsh = xonsh_with_pkgs; };
in finalOverlay
