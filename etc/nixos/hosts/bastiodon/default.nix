{ config, ... }:

{
    imports = [ ../../devices/rpi3.nix ];
    services.tailscale = {
        exitNode.advertise = true;
        acceptDNS = ! config.services.tailscale.exitNode.advertise;
    };
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "d1f560b3";
    };
    variables.relay = true;
}
