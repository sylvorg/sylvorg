{ config, ... }:

{
    imports = [ ../../minimal.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "afb6cebf";
    };
}
