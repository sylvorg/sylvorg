let
    name = "20220208045251000973158";
    pkgs = import <nixpkgs> {};
    venv = "~/.local/nix-shells/${name}/venv";
in (pkgs.mkShell rec {
    inherit name;
    buildInputs = with pkgs; [ python310 sd gcc rsync ];
    nativeBuildInputs = buildInputs;
    shellHook = ''
        python3 -m venv ${venv}
        source ${venv}/bin/activate
        pip install --upgrade pip || :
        pip install https://github.com/syvlorg/bakery/archive/main.tar.gz \
                    xonsh[full] || :
        chmod +x ${builtins.toString ./.}/nichtstrap 2> /dev/null || chmod +x ${builtins.toString ./.}/nichtstrap.py
        exec xonsh
    '';
})
