{ config, ... }:

{
    imports = [ ../../devices/rpi4.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "828973ce";
    };
}
