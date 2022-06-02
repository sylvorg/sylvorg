{ config, ... }:

{
    imports = [ ../../devices/rpi3.nix ];
    services.tailscale.advertiseExitNode = true;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "313ce470";
    };
    variables.relay = true;
}
