with builtins; { pkgs, lib, inputs ? {}, system ? currentSystem }: with lib; let
    newLib = self: rec {

        # TODO: Is this necessary?
        mntConvert = dir: let mntDir = "/mnt/" + dir; in if (pathExists mntDir) then mntDir else dir;

        zipToSet = names: values: listToAttrs (
            map (nv: nameValuePair nv.fst nv.snd) (let hasAttrs = any isAttrs values; in zipLists (
                if hasAttrs then names else (sort lessThan names)
            ) (
                if hasAttrs then values else (sort lessThan values)
            ))
        );

        # TODO: Why is the filter necessary?
        foldToSet = list: foldr (new: old: recursiveUpdate new old) {} (filter isAttrs list);
        foldToSet' = list: foldr (new: old: new // old) {} (filter isAttrs list);

        mif = {
            list = condition: value: optionals condition value;
            list' = condition: value: optional condition value;
            set = condition: value: optionalAttrs condition value;
            num = condition: value: if condition then value else 0;
            null = condition: value: if condition then value else null;
            str = condition: value: optionalString condition value;
            drv = condition: value: if condition then value else pkgs.hello;
        };
        readDirExists = dir: mif.set (pathExists dir) (readDir dir);
        toCapital = string: concatImapStrings (
            i: v: if (i == 0) then (toUpper v) else v
        ) (stringToCharacters string);
        sequence = list: end: foldr (a: b: deepSeq a b) end list;
        filters = {
            remove = {
                prefix = ignores: list: filter (f: ! any (i: hasPrefix i f) ignores) list;
                suffix = ignores: list: filter (f: ! any (i: hasSuffix i f) ignores) list;
                infix = ignores: list: filter (f: ! any (i: hasInfix i f) ignores) list;
            };
        };
        recurseDir = { dir, local ? false, iter ? 0, ignores ? {} }: with lib; let
            stringDir = toString dir;
            redDir = readDirExists dir;
            recurse = unique (flatten [
                (map (n: "${stringDir}/${n}") (attrNames (filterAttrs (n: v: v != "directory") redDir)))
                (map (dir': recurseDir { dir = "${stringDir}/${dir'}"; inherit local; iter = iter + 1; }) (attrNames (filterAttrs (n: v: v == "directory") redDir)))
            ]);
            processed-prefix = map (i: if (local == null) then i else if (local == 0) then "/${i}" else if local then "./${i}" else if (! local) then "${stringDir}/${i}" else i) (ignores.prefix or []);
            process' = recurse': let
                stringDir' = "${stringDir}/";
            in if (local == null) then (map (f: replaceStrings [ stringDir' ] [ "" ] f) recurse') else if (local == 0) then (map (f: replaceStrings [ stringDir' ] [ "/" ] f) recurse') else if local then (map (f: replaceStrings [ stringDir' ] [ "./" ] f) recurse') else recurse';
            process = recurse': with filters.remove; pipe recurse' [ process' (prefix processed-prefix) (suffix (ignores.suffix or [])) (infix (ignores.infix or [])) ];
        in if (iter == 0) then (process recurse) else recurse;
        has = {
            prefix = prefixes: string: any (prefix: hasPrefix prefix string) prefixes;
            suffix = suffixes: string: any (suffix: hasSuffix suffix string) suffixes;
            infix = infixes: string: any (infix: hasInfix infix string) infixes;
        };
        import = let
            args = {
                suffix = "";
                ignores = [];
            };
            baseNameNoSuffix = {
                suffix ? args.suffix,
                noSuffix ? suffix == "",
                file
            }: removeSuffix (if noSuffix then ".nix" else suffix) (baseNameOf (toString file));
            # !!! This returns a function
            filterFunc = {
                suffix ? args.suffix,
                noSuffix ? suffix == "",
                ignores ? args.ignores,
                dir,
            }: let
                _ignores = flatten [
                    ignores
                    (let _ignores' = (/. + "/${unsafeDiscardStringContext dir}/_ignores.nix"); in mif.list (pathExists _ignores') (import _ignores'))
                    "default" # triggers infinite recursion if modules are defined here
                    "deprecated"
                    "nix" # niv
                    "shell" # nix-shell
                ];
            in file: value:
                (if noSuffix then (
                    (hasSuffix ".nix" file) || (value == "directory")
                ) else (hasSuffix suffix file)) &&
                (!hasPrefix "_" file) &&
                (!elem (baseNameNoSuffix { inherit suffix file noSuffix; }) _ignores);
            contents = _args@{
                suffix ? args.suffix,
                ignores ? args.ignores,
                dir,
            }: filterAttrs (filterFunc _args) (readDir dir);
        in {
            list = _args@{
                suffix ? args.suffix,
                ignores ? args.ignores,
                dir,

                # !!! This cannot be integrated into the "args" set above, due to "dir" then being permanently set !!!
                func ? (n: v: dir + "/${n}"),

            }: let
                __args = removeAttrs _args [ "func" ];
            in mapAttrsToList func (contents __args);
            listNames = {
                suffix ? args.suffix,
                ignores ? args.ignores,
                dir
            }: list {
                inherit suffix ignores dir;
                func = (n: v: baseNameNoSuffix { inherit suffix; file = n; });
            };
            set = _args@{
                suffix ? args.suffix,
                ignores ? args.ignores,
                dir,
                modules ? {},

                # !!! This cannot be integrated into the "args" set above, due to "dir" then being permanently set !!!
                func ? (n: v: dir + "/" + n),

            }: let
                files = list (filterAttrs (arg: v: !elem arg [ "modules" "self" ]) _args);
            in listToAttrs (map (file: nameValuePair
                (baseNameNoSuffix { inherit suffix file; })
                (import file (foldToSet [ modules inputs ]))
            ) files);
        };
        attrs = rec {
            configs = {
                nixpkgs = {
                    allowUnfree = true;
                    allowBroken = true;
                    allowUnsupportedSystem = true;
                    # preBuild = ''
                    #     makeFlagsArray+=(CFLAGS="-w")
                    #     buildFlagsArray+=(CC=cc)
                    # '';
                    permittedInsecurePackages = [
                        "python2.7-cryptography-2.9.2"
                    ];
                };
                nix = let
                    MG = size: let
                        mg = stringToCharacters size;
                    in toString ((toInt (elemAt mg 0)) * (
                        if (elemAt mg 1 == "M") then 1 else 1024
                    ) * 1024 * 1024);
                in ''
                    # extra-substituters = https://cache.nixos.org/ https://nix-community.cachix.org/
                    trusted-substituters = https://cache.nixos.org/
                    # extra-trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
                    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
                    keep-derivations = true
                    keep-outputs = true
                    extra-experimental-features = nix-command flakes
                    allow-unsafe-native-code-during-evaluation = true
                    min-free = 262144000
                    max-free = 1073741824
                    min-free = ${MG "250M"}
                    max-free = ${MG "1G"}
                '';
                services = rec {
                    mkBase = User: {
                        enable = true;
                        serviceConfig = rec {
                            Restart = "on-failure";
                            inherit User;
                            Group = User;
                            Environment = [ "PATH=/run/wrappers/bin:$PATH" ];
                        };
                        wantedBy = [ "multi-user.target" ];
                    };
                    base = mkBase users.primary;
                    mkdir = path: "/run/current-system/sw/bin/mkdir -p ${path} &> /dev/null";
                };
            };
            users = fromJSON (readFile ./users.json);
            excludedUsers = [ "root" ];
            mainUsers = attrValues users;
            allUsers = mainUsers ++ excludedUsers;
            homes = fromJSON (readFile ./homes.json);
            allHomes = recursiveUpdate homes { root = "/root"; };
            datasets = {
                fileSystems = import ./datasets.nix host;
                backup = [
                    "system/persist"
                    "virt"
                    "omniverse"
                    users.primary
                ];
            };
            platforms = {
                arm = [ "aarch64-linux" "armv7l-linux" "armv6l-linux" ];
                imd = [ "i686-linux" "x86_64-linux" ];
            };
            arms = elem system platforms.arm;
            no-arms = !arms;
            ssh.keys = rec {
                "id_rsa.bak" = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDlwBJ7E2qeqw9kMW19indbeLdnEKs/Yrhn9HE0c/gZDzvXYBPQYyf5xr9I9kYxWcHlqp7XEI0LVT4DCA/mgemQtM8ulc1mxwekKtk64uWRi5wLi1E17NWKJfXWRn8XZejwi0iJa0twwVE8m8G2AuFOCSa86sYD3x5X5W+7spAuNET7kl0DLueUHu1u31c7HE1ciV2tIn/f60/bbgEJm9MPcRVZkRxkp+bouaZ1cjWRYDhvyJS30DRhBYtIIort2XVAshQs2Y58oKeCDnjt0gxotfqqWlt4nTQzKtbSN2M6/M+clFQBdT1oUJqpTUJbVxK8+xSEOJcBubupTj0USpmftDf/3WMoMwq+hNEc9C0EN1BYtKk68QWhAz8NROvnx7h6y3UKejhQOg0ueNZggmeNJLbebEs46QmA92khO8zc2pfBRsEa5yP0IgdvWpruTZ1QwjqhGQqGnCw3Oli1PK+5zgT2vXy5yHl3f3duPq8h+LOc+lSBbi2jjkC0gwTQDDDNyzFZ+U9xF7fCmL3V8DCEeO/4HqVxmLJir2TVEDo/3Ug/Q22Yp7P2EZrI2pikZIyBJc5aZJO3d7nGoDB/1BJp9Qm82wvyEpjiOnxHsL4osUqrf401XbiwNqpFkVUoRZkwGraJnrlsYkdHS2Mrrny9sr+PtgZhTjqIuW8z6iVIfQ== titaniumfiles@outlook.com";
                "id_ed25519.bak" = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO8NzKV52dRBAir8ARoFJX/xQDVCNup6xe1ddX1YVXSO sylvorg@syvl.org";
                jeet_ray_ecdsa = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBGsRy6rLDzmLNISdWahFLGDo+ZZLbndj6k8Q8MUQum/mPAzy8lsAQz/0XiicJz7LlM74tWGDYSJG1Ay2Iyc/ew4= jeet.ray@syvl.org";
                jeet_ray_ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICor+WXoAypnk5rkgTljAN6kk8olvKWqtnmGWVuQu8z9 jeet.ray@syvl.org";
                jeet_ray_rsa = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCs4zqEt/Fkjw0LVQTwJXlovmnqqGWC4UOVPvoLDvo0JD6WeVBDi4cFPX2mpNJYmYJsBLDXeUq5XrQ1ST3BkfVdspsragnD7O92tTEf3/VHfIC1L165pnB08FXQrtIjyLL7Ry4dloUGBYKLnHOtnXlpefKMQzRYUacc7Tr1o2wv+XRoDW9h+qDqJz1O61N68JFLgJWD3/nUkm8siTg1OLvqO9ATp+UgP/Lb08E6HfqYOiD8H+1ZJjz78mo5oZatknvgy8uJJPqEX7/aRM61YA9TG+tw/sf6wlrDtUQUik8Y4k1DLmkhE15wcgq/HF2Rqka/acA9GxA5smNGyjs6CS+H jeet.ray@syvl.org";
                shadowrylander_ecdsa = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBNlVuY9reRuMloYvecJHHsOYkAPDyQwELOI3kfibslIKI5hY+o1jx5yVyAUomHynP6wulm5aziNc5kWdsRE9BE8= shadowrylander@syvl.org";
                shadowrylander_ed25519 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINZ2FCMbnetAnDZ63Wzct+O3MYhtO9+BedATbtiHI9BT shadowrylander@syvl.org";
                shadowrylander_rsa = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDP8ifT/3d6L2MzZPoTh0bUjQUXuPKfPP8Tp03E5En2x+iKhv+J/U0z0xk7IdPZ4qEO+ZWI5xWbwVkDdnRnJ/5HgA0/ZwmO5Zpj3llSr4dJMUVSSyO23fFIL2WqOpHyQDeexJWMxbU5SmIi+c855VwewCbGDcPnmDo0XgR/u4LRF2pwYGNGFtJ2/GICEIob/2w0ICwi7TMUEkDbUFcP5web81OzsNu80M60VaNl870uT1rwBeKuW7CXFtImYytZ0mOc5LC6d7ugkFS1zAbLOWjt3PJ8Op2MH9ncBj5jCsIlA/OqI72jKwEPOl8evYqWeEOzlVxA7/AkRj7haQqFE8r/ shadowrylander@syvl.org";
                id_rsa = shadowrylander_rsa;
                id_ed25519 = jeet_ray_ed25519;
                id_ecdsa = jeet_ray_ecdsa;
            };
            fileSystems = {
                base = {
                    fsType = "zfs";
                    options = [ "defaults" "x-systemd.device-timeout=5" "nofail" ];
                };
                supported = [ "zfs" "xfs" "btrfs" "ext4" "fat" "vfat" "bcachefs" ];
            };
            commands = {
                rebuild = "nixos-rebuild --show-trace";
                install = "nixos-install --show-trace";
            };
        };
    };
    extension = makeExtensible newLib;
in with lib; extension.extend (final: prev: extension.foldToSet (attrValues prev))
