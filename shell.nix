with builtins; let
    flake = import ./etc/nixos;
    inherit (flake.make.specialArgs null currentSystem) nixpkgset lib;
    pkgs = import flake.inputs.nixpkgs nixpkgset;
in with pkgs; let
    strapper = { python, fetchFromGitHub }: python.pkgs.buildPythonApplication rec {
        pname = "strapper";
        version = "1.0.0.0";
        src = "./${pname}";
        propagatedBuildInputs = with python.pkgs; [ bakery ];
        dontBuild = true;
        installPhase = ''
            mkdir --parents $out/bin
            cp $src/${pname}.py $out/bin/${pname}
            cp $src/${pname}.hy $out/bin/
            chmod +x $out/bin/${pname}
            patchShebangs $out/bin/${pname}
        '';
        postInstall = "wrapProgram $out/bin/${pname} $makeWrapperArgs";
        makeWrapperArgs = [ "--prefix PYTHONPATH : ${placeholder "out"}/lib/${python.pkgs.python.libPrefix}/site-packages" ];
    };
in mkShell rec {
    buildInputs = [ (callPackage strapper {}) sd gcc rsync ];
    nativeBuildInputs = buildInputs;
}
