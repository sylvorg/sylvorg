with builtins; let flake = import ./etc/nixos;
in with import flake.inputs.nixpkgs (flake.make.specialArgs null currentSystem).nixpkgset; mkShell rec {
    buildInputs = [ python310 sd gcc rsync ] ++ (with python310Packages; [ bakery hy ]);
    nativeBuildInputs = buildInputs;
    shellHook = ''
        chmod +x ${toString ./.}/nichtstrap 2> /dev/null || chmod +x ${toString ./.}/nichtstrap.py
    '';
}
