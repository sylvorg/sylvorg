{ config, ... }:

{
    imports = [
        ../../super.nix
        ../../bcachefs.nix
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "2104f1bb";
    };
}
