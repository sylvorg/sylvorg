let
    name = "nichtstrap";
in with import <nixpkgs> {};
stdenv.mkDerivation rec {
  inherit name;
  buildInputs = with pkgs; [ python310 sd gcc rsync ];
  nativeBuildInputs = buildInputs;
  shellHook = ''
    python3 -m venv ~/.local/${name}
    source ~/.local/${name}/bin/activate
    pip install https://github.com/shadowrylander/bakery/archive/main.tar.gz \
                coconut \
                cytoolz \
                xonsh || :
    chmod +x ${builtins.toString ./.}/nichtstrap 2> /dev/null || chmod +x ${builtins.toString ./.}/nichtstrap.py
    exec xonsh
  '';
}
