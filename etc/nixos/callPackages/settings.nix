{ stdenv, fetchFromGitHub }: stdenv.mkDerivation rec {
    pname = "settings";
    version = "1.0.0.0";

    src = fetchFromGitHub {
        owner = "sylvorg";
        repo = "settings";
        rev = "3ce398e9eb5bd054526366d0965119c83265efe2";
    	sha256 = "sha256-sGof/jMtRcZky3mAHUCXI9w+JcDuSWbUGcGYxrybBzM=";
    };

    phases = [ "installPhase" ];

    installPhase = ''
        mkdir --parents $out
        cp -r $src/bin $out/bin
        chmod +x $out/bin/*
    '';

    meta.mainprogram = "org-tangle";
}
