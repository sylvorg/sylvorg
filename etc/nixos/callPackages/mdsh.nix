{ stdenv, fetchgit, lib }: stdenv.mkDerivation rec {
    pname = "mdsh";
    version = "1.0.0.0";

    src = fetchgit {
        url = "https://github.com/bashup/mdsh.git";
        rev = "7e7af618a341eebd50e7825b062bc192079ad5fc";
        sha256 = "sha256-l03BHDHjlhwRepVDW9FnKCAXZ8AC6pgHQdQJtYOP5fE=";
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
