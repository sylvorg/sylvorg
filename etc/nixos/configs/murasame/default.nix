{ config, ... }:

{
    imports = import ../imports.nix;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "49db07e7";
    };
}
