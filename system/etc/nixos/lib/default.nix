inputs@{
    sources ? ({
        nixpkgs = <nixpkgs>;
    }),
    pkgs ? (import sources.nixpkgs {}),
    lib ? pkgs.lib,
    preattrs ? (import ./_preattrs.nix lib),
    prelib ? (import ./_prelib.nix preattrs lib),
    primprelib ? (import ./_primprelib.nix preattrs prelib lib),
    ...
}: with builtins; with lib; let
    templib = self: prelib
        // (with prelib; {
            attrs = preattrs // (import ./_attrs.nix (inputs // { inherit preattrs primprelib; }));
            imprelib = primprelib // (import ./_imprelib.nix (inputs // { inherit primprelib; }));
        })
        // (listToAttrs (map (file: nameValuePair
            file
            (import (./. + "/${file}.nix") inputs)
        ) (primprelib.listNames { dir = ./.; })));

    __ = makeExtensible templib;
in __.extend (final: prev: prelib.foldToSet (attrValues prev))
