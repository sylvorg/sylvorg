{ config, pkgs, lib, ... }:

{
    imports = [ ../../devices/linode.nix ../../minimal.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "aab1a5fa";
    };
    variables.relay = true;
}
