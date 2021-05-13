{ config, lib, ... }: with builtins; with lib; with j; {
    imports = flatten [
        [ ../arm ../shared/armv67l.nix ]
        (imprelib.list { dir = ./.; })
    ];
}
