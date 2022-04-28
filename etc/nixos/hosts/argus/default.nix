{ config, pkgs, lib, ... }:

{
    imports = [ ../../devices/linode.nix ../../minimal.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "e34c3e56";
    };
}
