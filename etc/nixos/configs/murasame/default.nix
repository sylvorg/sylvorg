{ config, ... }:

{
    imports = [
        ../../super.nix
        ../../bcachefs.nix
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "340dcb14";
    };
}
