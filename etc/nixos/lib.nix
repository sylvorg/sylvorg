pkgs: lib: host: with lib; with builtins;
let
newLib = self: rec {
functions = rec {
zipToSet = names: values: listToAttrs (
    map (nv: nameValuePair nv.fst nv.snd) (let hasAttrs = any isAttrs values; in zipLists (
        if hasAttrs then names else (sort lessThan names)
    ) (
        if hasAttrs then values else (sort lessThan values)
    ))
);
# TODO: Why is the filter necessary?
foldToSet = list: foldr (new: old: recursiveUpdate new old) {} (filter (item: isAttrs item) list);
myIf = {
    list = condition: value: optionals condition value;
    singleton = condition: value: optional condition value;
    set = condition: value: if condition then value else {};
    num = condition: value: if condition then value else 0;
    knull = condition: value: if condition then value else null;
    empty = condition: value: if condition then value else "";
    drv = condition: value: if condition then value else pkgs.hello;
};
toCapital = string: concatImapStrings (
    i: v: if (i == 0) then (toUpper v) else v
) (stringToCharacters string);
sequence = list: end: foldr (a: b: deepSeq a b) end list;
genPersistentFD = filtered: persistentDirectory: let
    _ = type: filter (n: !elem n filtered) (
        mapAttrsToList (n: v: removePrefix persistentDirectory n)
    (filterAttrs (n: v: v == type) (readDir persistentDirectory)));
in { directories = _ "directories"; files = _ "regular"; };
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
        (let
            _ignores' = (/. + "/${unsafeDiscardStringContext dir}/_ignores.nix");
        in if (pathExists _ignores') then (import _ignores') else [])
        [ "default" "deprecated" ]
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
in zipToSet
    (map (file: name { inherit suffix file; }) files)
    (map (file: import file (foldToSet [ modules inputs ])) files);
};
attrs = {
persistent = {
    files = {
        system = flatten [[ "/etc/host" ]];
    };
    directories = {
        system = flatten [[
            "/etc/containers"
            "/etc/NetworkManager/system-connections"
            "/etc/nix"
            "/etc/nixos"
            "/etc/ssh"
            "/etc/wireguard"
            "/var/lib/acme"
            "/var/lib/bluetooth"
            "/usr"
            "/bin"
            "/sbin"
            "/snap"
        ]];
    };
};
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
        keep-derivations = true
        keep-outputs = true
        experimental-features = nix-command flakes
        allow-unsafe-native-code-during-evaluation = true
        min-free = ${MG "250M"}
        max-free = ${MG "1G"}
    '';
    services = rec {
        mkBase = User: {
            enable = true;
            serviceConfig = {
                Restart = "on-failure";
                inherit User;
            };
            wantedBy = [ "multi-user.target" ];
        };
        base = mkBase "shadowrylander";
        mkdir = path: "mkdir -p /persist/${path} &> /dev/null";
    };
};
users = fromJSON (readFile ./users.json);
excludedUsers = [ "root" ];
mainUsers = attrValues users;
allUsers = mainUsers ++ excludedUsers;
homes = listToAttrs (map (
    user: nameValuePair user "/home/${user}"
) mainUsers);
allHomes = homes // { root = "/root"; };
datasets = {
    fileSystems = import /etc/nixos/datasets.nix;
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
ssh.keys = {
    master = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDlwBJ7E2qeqw9kMW19indbeLdnEKs/Yrhn9HE0c/gZDzvXYBPQYyf5xr9I9kYxWcHlqp7XEI0LVT4DCA/mgemQtM8ulc1mxwekKtk64uWRi5wLi1E17NWKJfXWRn8XZejwi0iJa0twwVE8m8G2AuFOCSa86sYD3x5X5W+7spAuNET7kl0DLueUHu1u31c7HE1ciV2tIn/f60/bbgEJm9MPcRVZkRxkp+bouaZ1cjWRYDhvyJS30DRhBYtIIort2XVAshQs2Y58oKeCDnjt0gxotfqqWlt4nTQzKtbSN2M6/M+clFQBdT1oUJqpTUJbVxK8+xSEOJcBubupTj0USpmftDf/3WMoMwq+hNEc9C0EN1BYtKk68QWhAz8NROvnx7h6y3UKejhQOg0ueNZggmeNJLbebEs46QmA92khO8zc2pfBRsEa5yP0IgdvWpruTZ1QwjqhGQqGnCw3Oli1PK+5zgT2vXy5yHl3f3duPq8h+LOc+lSBbi2jjkC0gwTQDDDNyzFZ+U9xF7fCmL3V8DCEeO/4HqVxmLJir2TVEDo/3Ug/Q22Yp7P2EZrI2pikZIyBJc5aZJO3d7nGoDB/1BJp9Qm82wvyEpjiOnxHsL4osUqrf401XbiwNqpFkVUoRZkwGraJnrlsYkdHS2Mrrny9sr+PtgZhTjqIuW8z6iVIfQ== titaniumfiles@outlook.com";
};
fileSystems = {
    base = {
        fsType = "zfs";
        options = [ "defaults" "x-systemd.device-timeout=5" "nofail" ];
    };
    supported = [ "zfs" "xfs" "btrfs" "ext4" "fat" "vfat"  ];
};
commands = {
    rebuild = "nixos-rebuild --impure";
    install = "nixos-install --impure --show-trace";
};
versions = {
    python = "310";
    emacs = "28";
};
};
paths = rec {
    nixos = toString (./. + "/..");
    lib = "${nixos}/lib";
    extras = "${nixos}/extras";
    configs = "${nixos}/configs";
    modules.flakes = "${nixos}/flake_modules";
    global = "${nixos}/global";
    patches.base = "${nixos}/patches";
};
};
extension = makeExtensible newLib;
in with lib; extension.extend (final: prev: foldr (new: old: recursiveUpdate new old) {} (attrValues prev))
