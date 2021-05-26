{ config, lib, ... }: with builtins; with lib; with j; {
    imports = flatten [
        (imprelib.list { dir = ./.; })
        (imprelib.list { dir = ../../config; })
    ];
    config.networking.hostId = "4a06d3c7";
}
