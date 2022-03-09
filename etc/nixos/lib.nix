{ pkgs, lib, inputs ? {}, host ? "nixos" }: with lib; with builtins;
let
newLib = self: rec {
functions = rec {
mntConvert = dir: let mntDir = "/mnt/" + dir; in if (pathExists mntDir) then mntDir else dir;
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
attrs = rec {
relays = [ "argus" "bastiodon" ];
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
        extra-substituters = https://cache.nixos.org https://hydra.nixos.org https://nrdxp.cachix.org https://nix-community.cachix.org
        extra-trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs= nrdxp.cachix.org-1:Fc5PSqY2Jm1TrWfm88l6cvGWwz3s93c6IOifQWnhNW4= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
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
homes = listToAttrs (map (
    user: nameValuePair user "/home/${user}"
) mainUsers);
allHomes = homes // { root = "/root"; };
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
