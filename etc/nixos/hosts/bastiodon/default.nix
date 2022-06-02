{ config, ... }:

{
    imports = [ ../../devices/rpi3.nix ];
    services.tailscale.advertiseExitNode = true;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "0ad7a4d1";
    };
    variables.relay = true;
}
