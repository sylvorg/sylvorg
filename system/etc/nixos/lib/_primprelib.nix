preattrs: prelib: lib:
with builtins;
with preattrs;
with prelib;
with lib;
rec {
    args = {
        suffix = "";
        ignores = [];
    };

    name = {
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
        (!elem (name { inherit suffix file noSuffix; }) _ignores);

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
        func = (n: v: name { inherit suffix; file = n; });
    };
}
