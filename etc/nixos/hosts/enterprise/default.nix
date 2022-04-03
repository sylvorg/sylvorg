{ config, ... }:

{
    imports = [ ../../devices/rpi4.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "c24557a1";
    };
}
