{ config, pkgs, lib, ... }:

{
    imports = [ ../../devices/linode.nix ../../minimal.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "333ee29e";
    };
    variables.relay = true;
}
