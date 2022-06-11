{ config, ... }:

{
    imports = [ ../../devices/rpi4.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "9c4dc664";
    };
    variables.zfs = false;
}
