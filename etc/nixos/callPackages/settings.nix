{ stdenv, fetchFromGitHub }: stdenv.mkDerivation rec {
    pname = "settings";
    version = "1.0.0.0";

    src = fetchTarball {
        url = "https://github.com/sylvorg/${pname}/archive/3ce398e9eb5bd054526366d0965119c83265efe2.tar.gz";
    	sha256 = "0cq7kfycd66137a6cjgfq0jkxp13jx01v03rrdjcci9d6gz1ysmh";
    };

    phases = [ "installPhase" ];

    installPhase = ''
        mkdir --parents $out
        cp -r $src/bin $out/bin
        chmod +x $out/bin/*
    '';

    meta.mainprogram = "org-tangle";
}
