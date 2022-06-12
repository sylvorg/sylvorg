with builtins; let
    flake = import ./etc/nixos;
    pkgs = import flake.inputs.nixpkgs (flake.make.specialArgs null currentSystem).nixpkgset;
in with pkgs; let
    strapper = { python3, fetchFromGitHub }: python3.pkgs.buildPythonApplication rec {
        pname = "strapper";
        version = "1.0.0.0";
        src = ./strapper;
        propagatedBuildInputs = with python3.pkgs; [ bakery ];
        dontBuild = true;
        installPhase = ''
            mkdir --parents $out/bin
            cp $src/strapper.py $out/bin/strapper
            cp $src/strapper.hy $out/bin/
            chmod +x $out/bin/strapper
            patchShebangs $out/bin/strapper
        '';
        postInstall = "wrapProgram $out/bin/strapper $makeWrapperArgs";
        makeWrapperArgs = [ "--prefix PYTHONPATH : ${placeholder "out"}/lib/${python3.pkgs.python.libPrefix}/site-packages" ];
    };
in mkShell rec {
    buildInputs = [ (callPackage strapper {}) sd gcc rsync ];
    nativeBuildInputs = buildInputs;
}
