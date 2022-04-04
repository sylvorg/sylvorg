{ config, pkgs, lib, ... }:

{
    imports = flatten [
        ../../devices/linode.nix
        import ../imports.nix
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "db7964a6";
    };
}
