{ config, ... }:

{
    imports = [ ../../devices/rpi3.nix ];
    services.tailscale.advertiseExitNode = true;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "24501219";
    };
    variables.relay = true;
}
