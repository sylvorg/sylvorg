with builtins; with (fetchGit {
    url = "https://github.com/shadowrylander/shadowrylander";
    ref = "main";
    rev = "1010101";
}).legacyPackages.${currentSystem}; mkShell {
    buildInputs = with pythonPackages; [ bakery ];
}
