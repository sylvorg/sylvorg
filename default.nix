with builtins; let
    flakePath = "etc/nixos/flake.nix";
    etc = {
        root = rec {
            path = "/${flakePath}";
            exists = pathExists path;
        };
        local = rec {
            path = "${toString ./.}/${flakePath}";
            exists = pathExists path;
        };
    };
    userrepo = rec {
        path = "/home/shadowrylander/aiern/${flakePath}";
        exists = pathExists path;
    };
    home = rec {
        path = "/home/shadowrylander/${flakePath}";
        exists = pathExists path;
    };
    yadm = rec {
        path = "/home/shadowrylander/${flakePath}";
        exists = pathExists path;
    };
    flake = if etc.local.exists then (dirOf etc.local.path)
            else if userrepo.exists then (dirOf userrepo.path)
            else if etc.root.exists then (dirOf etc.root.path)
            else if yadm.exists then (dirOf yadm.path)
            else if home.exists then (dirOf home.path)
            else (fetchGit {
                url = "https://github.com/shadowrylander/shadowrylander";
                ref = "main";
            });
in import flake
