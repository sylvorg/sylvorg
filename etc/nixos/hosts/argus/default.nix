{ config, pkgs, lib, ... }:

{
    imports = flatten [
        ../../devices/linode.nix
        import ../imports.nix
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "420383ed";
    };
}
