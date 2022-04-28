{ config, pkgs, lib, ... }:

{
    imports = [ ../../devices/linode.nix ../../minimal.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "969a9c27";
    };
}
