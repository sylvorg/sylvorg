{ config, ... }:

{
    imports = import ../imports.nix;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "24d08f5d";
    };
}
