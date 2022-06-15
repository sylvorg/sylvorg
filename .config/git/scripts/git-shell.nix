with builtins; with (import (fetchGit {
    url = "https://github.com/shadowrylander/shadowrylander";
    ref = "main";
})).legacyPackages.${currentSystem}; mkShell {
    buildInputs = [ git-crypt git-filter-repo rsync realpath ];
}
