{ config, ... }:

{
    imports = [ ../../minimal.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "cb476b1d";
    };
}
