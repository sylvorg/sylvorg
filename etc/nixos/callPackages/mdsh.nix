{ stdenv, fetchFromGitHub, lib }: stdenv.mkDerivation rec {
    pname = "mdsh";
    version = "1.0.0.0";

    src = fetchTarball {
        url = "https://github.com/bashup/${pname}/archive/7e7af618a341eebd50e7825b062bc192079ad5fc.tar.gz";
        sha256 = "1wg5iy1va2fl843rish2q1kif818cz8mnhwmg88ir5p364fc2kcp";
    };

    installPhase = ''
        mkdir --parents $out/bin
        cp "$src/bin/mdsh" $out/bin/
    '';

    meta = {
        description = "Multi-lingual, Markdown-based Literate Programming... in run-anywhere bash";
        homepage = "https://github.com/bashup/mdsh";
        license = lib.licenses.mit;
    };
}
