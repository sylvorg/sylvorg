{ config, ... }:

{
    imports = [ ../../minimal.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "e375fc3d";
    };
}
