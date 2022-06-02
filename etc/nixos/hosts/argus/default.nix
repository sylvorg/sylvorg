{ config, pkgs, lib, ... }:

{
    imports = [ ../../devices/linode.nix ../../minimal.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "0b431461";
    };
    variables.relay = true;
}
