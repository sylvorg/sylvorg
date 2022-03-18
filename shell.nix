let flake = import ./etc/nixos;
in with import flake.inputs.nixpkgs { inherit (flake.specialArgs."${currentSystem}") overlays config; }; mkShell rec {
    buildInputs = [ python310 python310Packages.bakery sd gcc rsync ];
    nativeBuildInputs = buildInputs;
    shellHook = ''
        chmod +x ${builtins.toString ./.}/nichtstrap 2> /dev/null || chmod +x ${builtins.toString ./.}/nichtstrap.py
    '';
}
