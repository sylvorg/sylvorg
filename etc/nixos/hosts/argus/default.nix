{ config, pkgs, lib, ... }:

{
    imports = flatten [
        ../../devices/linode.nix
        import ../imports.nix
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "/home/shadowrylander/.zshenv:2: parse error near `\n'
        hostId =  5772d776";
    };
}
