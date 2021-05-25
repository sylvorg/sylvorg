inputs@{ config, lib, ... }: with builtins; with lib; with j; {
    imports = map (fd: import fd inputs) (flatten [
        [ ../arm ../shared/armv67l.nix ]
        (imprelib.list { dir = ./.; })
    ]);
}
