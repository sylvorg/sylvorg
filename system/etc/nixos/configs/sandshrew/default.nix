{ config, lib, ... }: with lib.j; {
    imports = flatten [
        (imprelib.list { dir = ./.; })
        (imprelib.list { dir = ../../config; })
    ];
    config.networking.hostId = "3c2fdcf3";
}
