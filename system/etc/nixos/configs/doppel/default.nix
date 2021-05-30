{ config, lib, ... }: with builtins; with lib; with j; {
    imports = flatten [
        (imprelib.list { dir = ./.; })
        (imprelib.list { dir = ../../config; ignores = [ "datasets" ]; })
    ];
    config.networking.hostId = "5661b2a2";
}
