inputs@{ config, lib, ... }: with builtins; with lib; with j; {
    imports = map (fd: import fd inputs) (flatten [
        [ ../arm ]
        (imprelib.list { dir = ./.; })
    ]);
}
