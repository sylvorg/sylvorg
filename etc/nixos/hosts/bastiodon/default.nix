{ config, ... }:

{
    imports = [ ../../devices/rpi3.nix ];
    services.tailscale = {
        exitNode.advertise = true;
        acceptDNS = ! config.services.tailscale.exitNode.advertise;
    };
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "da1ccbe8";
    };
    variables.relay = true;
}
