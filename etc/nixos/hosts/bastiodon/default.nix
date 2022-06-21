{ config, ... }:

{
    imports = [ ../../devices/rpi3.nix ];
    services.tailscale = {
        exitNode.advertise = true;
        acceptDNS = ! config.services.tailscale.exitNode.advertise;
    };
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "3e9a8073";
    };
    variables.relay = true;
}
