{ config, ... }:

{
    imports = [ ../rpi4.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "c8da1267";
    };
}
