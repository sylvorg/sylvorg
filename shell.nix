let
    name = "nichtstrap";
    pkgs = import <nixpkgs> {};
in (pkgs.buildFHSUserEnv {
    inherit name;
    targetPkgs = pkgs: with pkgs; [ python310 sd gcc rsync ];
    runScript = ''
        pip install --upgrade pip
        pip install https://github.com/shadowrylander/bakery/archive/main.tar.gz \
                    coconut \
                    cytoolz \
                    xonsh || :
        chmod +x ${builtins.toString ./.}/nichtstrap 2> /dev/null || chmod +x ${builtins.toString ./.}/nichtstrap.py
        exec xonsh
    '';
}).env
