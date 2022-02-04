let
    name = "nichtstrap";
    bakery = "~/.local/${name}/bakery";
in with import <nixpkgs> {};
stdenv.mkDerivation rec {
  inherit name;
  buildInputs = with pkgs; [ python310 sd gcc rsync ];
  nativeBuildInputs = buildInputs;
  shellHook = ''
    python3 -m venv ~/.local/${name}/venv
    source ~/.local/${name}/venv/bin/activate
    [ -d ${bakery} ] || git clone https://github.com/shadowrylander/bakery.git ${bakery}
    [ $(git -C ${bakery} fetch --dry-run | head -c 1 | wc -l) -ne 0 ] && git -C ${bakery} pull && pip install ${bakery}
    pip install coconut \
                cytoolz \
                xonsh
    chmod +x ${builtins.toString ./.}/nichtstrap
    exec xonsh
  '';
}
