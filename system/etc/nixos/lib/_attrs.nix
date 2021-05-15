{ lib, preattrs, primprelib, ... }:
with builtins;
with preattrs;
with primprelib;
with lib;
rec {
    persistent = {
        files = {
            system = flatten [[ "/etc/host" ]];
            home = flatten [[
                ".emacs-profile"
                ".gitignore"
                ".nix-channels"
                ".python-history"
                ".viminfo"
                ".zsh-history"
                "README.org"
                "LICENSE"
            ]];
            cache = flatten [[  ]];
        };
        directories = {
            system = flatten [[
                "/etc/containers"
                "/etc/NetworkManager/system-connections"
                "/etc/nix"
                "/etc/nixos"
                "/etc/nixos/config"
                "/etc/ssh"
                "/etc/wireguard"
                "/var/lib/acme"
                "/var/lib/bluetooth"
            ]];
            home = flatten [[
                ".atom"
                ".bash-history"
                ".byobu"
                ".config"
                ".linuxbrew"
                ".local"
                ".mozilla"
                ".peru"
                ".pki"
                ".vim_runtime"
                ".virtualenvs"
                ".vscode-oss"
                ".vscode"
                ".yubico"
                "Documents"
                "Downloads"
                "etc"
                "inca"
                "keybase"
                "Music"
                "nix-plugins"
                "Pictures"
                "Public"
                "Templates"
                "Videos"
                users.primary
            ]];
            cache = flatten [(map (dir: ".cache/${dir}") [
                "flatpak"
                "gnome-software"
                "google-chrome-beta"
                "google-chrome-dev"
                "google-chrome"
                "Homebrew"
                "keybase"
                "mozilla"
                "nix"
                "pip"
                "pypoetry"
                "qtile"
                "starship"
                "vivaldi"
            ])];
        };
    };

    persistence = {
        system = extraFD: let persistent-directory = "/persist"; in {
            "${persistent-directory}" = mkMerge [
                ({
                    directories = persistent.directories.system;
                    files = persistent.files.system;
                })
                (generatePersistentFD [ "home" "cache" ] persistent-directory)
                extraFD
            ];
        };
        home = { user ? "root", extraCache ? {}, extraFD ? {}}: let
            phu = "/persist/home/${user}";
        in mapAttrs (n: v: v // { allowOther = true; }) {
            "/persist/cache/${user}" = foldToSet [
                ({
                    directories = persistent.directories.cache;
                    files = persistent.files.cache;
                })
                extraCache
            ];
            "${phu}" = foldToSet [
                ({
                    directories = persistent.directories.home;
                    files = persistent.files.home;
                })
                (generatePersistentFD [""] phu)
                extraFD
            ];
        };
    };

    link = root: source: listToAttrs (map (
        fd: nameValuePair "${root}/${fd}" {
            source = "${source}/${fd}";
            recursive = true;
        }
    ) (attrNames (readDir source)));

    # Device Types
    types = flatten [
        (listNames { dir = ./devices; })
        [ "def" ]
    ];

    devices = [
        "4b"
        "3b"
        "pro"
        "b2"
        "def"
    ];

    hosts = listNames { dir = ./configs; };

    # hosts = listToAttrs (flatten [
    #     (map (host: nameValuePair host {}) [
    #         # "bastion"
    #         "chimchar"
    #         # "fell"
    #         # "flipper"
    #         # "infernape"
    #         # "monferno"
    #         # "murasame"
    #         # "p20"
    #         # "p5"
    #         # "piplup"
    #         # "sandshrew"
    #         "sandslash"
    #         # "siluam"
    #         # "silvester"
    #     ])
    # ]);

    configs = rec {
        prenix = ''
            keep-derivations = true
            keep-outputs = true
            experimental-features = nix-command flakes
            allow-unsafe-native-code-during-evaluation = true
        '';
        nix = let
            MG = size: let
                mg = stringToCharacters size;
            in toString ((toInt (elemAt mg 0)) * (
                if (elemAt mg 1 == "M") then 1 else 1024
            ) * 1024 * 1024);
        in prenix + ''
            min-free = ${MG "250M"}
            max-free = ${MG "1G"}
        '';
    };
    services = {
        base = {
            enable = true;
            serviceConfig = {
                Restart = "on-failure";
                User = "shadowrylander";
            };
            wantedBy = [ "multi-user.target" ];
        };
        mkdir = path: "mkdir -p /persist/${path} &> /dev/null";
    };
    users = fromJSON (readFile ../resources/users.json);
    excludedUsers = [ "root" ];
    mainUsers = attrValues users;
    allUsers = mainUsers ++ excludedUsers;
    homes = listToAttrs (map (
        user: nameValuePair user "/home/${user}"
    ) mainUsers);
    allHomes = homes // { root = "/root"; };
    commands = {
        rebuild = "nixos-rebuild --impure";
        install = "nixos-install --impure --show-trace";
    };
    fileSystems = {
        base = {
            fsType = "zfs";
            options = [ "defaults" "x-systemd.device-timeout=5" "nofail" ];
        };
        supported = [ "zfs" "xfs" "btrfs" "ext4" "fat" "vfat"  ];
    };
    versions = {
        python = "39";
        emacs = "26";
    };
    ssh.keys = {
        master = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDlwBJ7E2qeqw9kMW19indbeLdnEKs/Yrhn9HE0c/gZDzvXYBPQYyf5xr9I9kYxWcHlqp7XEI0LVT4DCA/mgemQtM8ulc1mxwekKtk64uWRi5wLi1E17NWKJfXWRn8XZejwi0iJa0twwVE8m8G2AuFOCSa86sYD3x5X5W+7spAuNET7kl0DLueUHu1u31c7HE1ciV2tIn/f60/bbgEJm9MPcRVZkRxkp+bouaZ1cjWRYDhvyJS30DRhBYtIIort2XVAshQs2Y58oKeCDnjt0gxotfqqWlt4nTQzKtbSN2M6/M+clFQBdT1oUJqpTUJbVxK8+xSEOJcBubupTj0USpmftDf/3WMoMwq+hNEc9C0EN1BYtKk68QWhAz8NROvnx7h6y3UKejhQOg0ueNZggmeNJLbebEs46QmA92khO8zc2pfBRsEa5yP0IgdvWpruTZ1QwjqhGQqGnCw3Oli1PK+5zgT2vXy5yHl3f3duPq8h+LOc+lSBbi2jjkC0gwTQDDDNyzFZ+U9xF7fCmL3V8DCEeO/4HqVxmLJir2TVEDo/3Ug/Q22Yp7P2EZrI2pikZIyBJc5aZJO3d7nGoDB/1BJp9Qm82wvyEpjiOnxHsL4osUqrf401XbiwNqpFkVUoRZkwGraJnrlsYkdHS2Mrrny9sr+PtgZhTjqIuW8z6iVIfQ== titaniumfiles@outlook.com";
    };
    platforms = {
        arm = [ "aarch64-linux" "armv7l-linux" "armv6l-linux" ];
        imd = [ "i686-linux" "x86_64-linux" ];
    };
}
