with builtins; let flake = import ./etc/nixos;
in with import flake.inputs.nixpkgs { inherit (flake.make.specialArgs null currentSystem) overlays config; }; mkShell rec {
    buildInputs = [ python310 python310Packages.bakery sd gcc rsync ];
    nativeBuildInputs = buildInputs;
    shellHook = ''
        chmod +x ${toString ./.}/nichtstrap 2> /dev/null || chmod +x ${toString ./.}/nichtstrap.py
    '';
}
