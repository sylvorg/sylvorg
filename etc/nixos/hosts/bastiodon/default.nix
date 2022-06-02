{ config, ... }:

{
    imports = [ ../../devices/rpi3.nix ];
    services.tailscale.advertiseExitNode = true;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "48836f16";
    };
    variables.relay = true;
}
