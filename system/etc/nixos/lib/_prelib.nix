preattrs: lib:
with builtins;
with preattrs;
with lib;
rec {

    get = {
        stc ? preattrs.default-stc,
        set,
        ignoredAttrs ? []
    } : getAttrFromPath (
        map toString (attrValues (removeAttrs stc ignoredAttrs))
    ) set;

    hostName = { stc, ... }: substring 0 61 (hashString "sha512" (concatStrings (
        map toString (attrValues stc)
    )));

    forAllSystems' = {
        func,
        all,
        extraListSets ? {},
        attrList ? preattrs.stc,
        inheritance ? {}
    } : let
        all' = (filterAttrs (n: v: elem n (flatten [
            attrList
            [ "host" ]
        ])) all) // extraListSets;
        product = cartesianProductOfSets all';
    in listToAttrs (map (
        stc: nameValuePair (hostName { inherit stc; }) (func ({ inherit stc; } // inheritance))
    ) product);

    forAllSystems = {
        func,
        all,
        extraListSets ? {},
        attrList ? preattrs.stc,
        inheritance ? {}
    } : let
        all' = (filterAttrs (n: v: elem n attrList) all) // extraListSets;
        product = cartesianProductOfSets all';
    in foldToSet (map (
        # !!! There is no need to alphabetically sort the output of attrValues! !!!
        stc: setAttrByPath
            (map toString (attrValues stc))
            (func ({ inherit stc; } // inheritance))
    ) product);

    # TODO: Why is the filter necessary?
    foldToSet = list: foldr (new: old: recursiveUpdate new old) {} (filter (item: isAttrs item) list);

    myIf = {
        list = condition: value: optionals condition value;
        singleton = condition: value: optional condition value;
        set = condition: value: if condition then value else {};
        num = condition: value: if condition then value else 0;
        knull = condition: value: if condition then value else null;
        empty = condition: value: if condition then value else "";
        drv = condition: evalue: value: if condition then value else evalue;
    };

    zipToSet = names: values: listToAttrs (
        map (nv: nameValuePair nv.fst nv.snd) (let hasAttrs = any isAttrs values; in zipLists (
            if hasAttrs then names else (sort lessThan names)
        ) (
            if hasAttrs then values else (sort lessThan values)
        ))
    );

    stdenv = {
        pkgs,
        stc ? ({ system = currentSystem; musl = 1; }),
        ...
    }: with stc; let
        musl = stc.musl == 1;
        arm = {
            systems = [
                "armv7a-linux"
                "armv7l-linux"
                "armv6a-linux"
                "armv6l-linux"
                "aarch64-linux"
            ];
            enable = elem system arm.systems;
            stdenv = {
                package = pkgs.clang10.stdenv;
                targetPlatform = if "armv7l-linux" then systems.examples.armv7l-hf-multiplatform
                    else systems.examples.aarch64-multiplatform;
            };
        };
        musl' = {
            enable = musl && (elem system ([
                "x86_64-linux"
            ] ++ arm.systems));
            stdenv = {
                package = pkgs.musl.stdenv;
                targetPlatform = systems.examples.musl64;
            };
        };
    in foldToSet [
        {
            inherit system;
            hostPlatform = currentSystem;
            buidPlatform = currentSystem;
        }
        (with arm; myIf.set enable stdenv)
        (with musl'; myIf.set enable stdenv)
    ];

    config = {
        pkgs,
        stc ? ({ system = currentSystem; musl = 1; }),
        buildEnvOvr ? true,
        stdenvs ? {},
        stdenv ? (attrByPath (attrValues stc) (stdenv { inherit pkgs stc; }) stdenvs),
        ...
    } : with stc; {
        inherit system;
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
        ${if buildEnvOvr then null else "buildEnv"} = {
            override = { inherit stdenv; };
        };
    };

    toCapital = string: concatImapStrings (
        i: v: if (i == 0) then (toUpper v) else v
    ) (stringToCharacters string);

    generatePersistentFD = filtered: persistentDirectory: let
        _ = type: filter (n: !elem n filtered) (
            mapAttrsToList (n: v: removePrefix persistentDirectory n)
        (filterAttrs (n: v: v == type) (readDir persistentDirectory)));
    in { directories = _ "directories"; files = _ "regular"; };

    sequence = list: end: foldr (a: b: deepSeq a b) end list;
}
