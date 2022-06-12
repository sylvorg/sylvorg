{ config, ... }:

{
    imports = [ ../../devices/rpi4.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "575b4023";
    };
    variables.zfs = false;
}
