{ stdenv, fetchFromGitHub }: with builtins; stdenv.mkDerivation rec {
    pname = "settings";
    version = "1.0.0.0";

    src = fetchFromGitHub {
        owner = "sylvorg";
        repo = "settings";
        rev = "3b8f7d1afe67f147807e48a510674b6088fc2af0";
    	sha256 = "1fn0pwv2pss6dslsy7ykbfaz43mplzdlamp8pwzkrk8mc9hk3ilf";
    };

    phases = [ "installPhase" ];

    installPhase = ''
        mkdir --parents $out
        cp -r $src/bin $out/bin
        chmod +x $out/bin/*
    '';
}
