{ config, ... }:

{
    imports = [ ../rpi4.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "f15f0dfa";
    };
}
