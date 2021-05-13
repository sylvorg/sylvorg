{ config, lib, ... }: with builtins; with lib; with j; {
    imports = flatten [
        [ ../arm ]
        (imprelib.list { dir = ./.; })
    ];
}
