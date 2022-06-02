{ config, ... }:

{
    imports = [ ../../devices/rpi3.nix ];
    services.tailscale.advertiseExitNode = true;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "50919b49";
    };
    variables.relay = true;
}
