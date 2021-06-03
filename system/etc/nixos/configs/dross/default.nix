{ config, lib, ... }: with builtins; with lib; with j; {
    imports = flatten [
        (imprelib.list { dir = ./.; })
        (imprelib.list { dir = ../../config; ignores = [ "datasets" ]; })
    ];
    config.networking.hostId = "b12f221c";
}
