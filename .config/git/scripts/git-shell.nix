with builtins; with (import "${(fetchGit {
    url = "https://github.com/shadowrylander/shadowrylander";
    ref = "main";
})}/etc/nixos").legacyPackages.${currentSystem}; mkShell {
    buildInputs = [ git-crypt git-filter-repo rsync realpath ];
}
