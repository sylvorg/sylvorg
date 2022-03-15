with import (import ./etc/nixos).inputs.nixpkgs {}; mkShell rec {
    buildInputs = [ python310 python310Packages.bakery xonsh sd gcc rsync ];
    nativeBuildInputs = buildInputs;
    shellHook = ''
        chmod +x ${builtins.toString ./.}/nichtstrap 2> /dev/null || chmod +x ${builtins.toString ./.}/nichtstrap.py
        exec xonsh
    '';
}
