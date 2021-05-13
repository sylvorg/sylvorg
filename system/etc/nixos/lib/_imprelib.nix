{ lib, primprelib, ... }:
with builtins;
with primprelib;
with lib;
rec {
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
}
