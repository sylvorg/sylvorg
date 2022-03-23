with builtins; let flake = import ./etc/nixos;
in with import flake.inputs.nixpkgs (flake.make.specialArgs null currentSystem).nixpkgset; let
    strapper = stdenv.mkDerivation rec {
        pname = "strapper";
        version = "1.0.0.0";

        src = ./strapper;

        buildInputs = [ python310 sd gcc rsync ] ++ (with python310Packages; [ bakery ]);
        nativeBuildInputs = buildInputs;

        phases = [ "installPhase" ];

        installPhase = ''
            mkdir --parents $out
            cp -r $src $out/bin
            chmod +x $out/bin/strapper
        '';
    };
in mkShell rec {
    buildInputs = [ strapper ];
    nativeBuildInputs = buildInputs;
}
