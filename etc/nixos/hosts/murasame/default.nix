{ config, ... }:

{
    imports = import ../imports.nix;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "ab5e236f";
    };
}
