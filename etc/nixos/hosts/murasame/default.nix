{ config, ... }:

{
    imports = import ../imports.nix;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "30c59913";
    };
}
