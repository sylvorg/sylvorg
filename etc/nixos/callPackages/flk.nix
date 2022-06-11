{ stdenv, fetchgit, lib }: stdenv.mkDerivation rec {
    pname = "flk";
    version = "1.0.0.0";

    src = fetchgit {
        url = "https://github.com/chr15m/flk.git";
        rev = "46a88bdb461dda336d5aca851c16d938e05304dc";
        sha256 = "sha256-NAhWe0O1K3LOdIwYNOHfkBzkGm+h0wckpsCuY/lY/+8=";
        deepClone = true;
    };

    installPhase = ''
        mkdir --parents $out/bin
        cp ./docs/flk $out/bin/
    '';

    meta = {
        description = "A LISP that runs wherever Bash is";
        homepage = "https://github.com/chr15m/flk";
        license = lib.licenses.mpl20;
    };
}
