with builtins; with (import "${(fetchGit {
    url = "https://github.com/shadowrylander/shadowrylander";
    ref = "main";
})}/etc/nixos").legacyPackages.${currentSystem}; mkShell {
    buildInputs = with pythonPackages; [ bakery ];
}
