{ config, lib, ... }: with lib; with j; {
    imports = flatten [
        (imprelib.list { dir = ./.; })
        (imprelib.list { dir = ../../config; })
    ];
    config.networking.hostId = "ed90aff6";
}
