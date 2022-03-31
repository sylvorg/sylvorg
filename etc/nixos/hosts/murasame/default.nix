{ config, ... }:

{
    imports = import ../imports.nix;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "c7a116f6";
    };
}
