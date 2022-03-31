{ config, ... }:

{
    imports = [ ../../devices/rpi4.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "da3ef2b5";
    };
}
