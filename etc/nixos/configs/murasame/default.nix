{ config, ... }:

{
    imports = [
        ../../super.nix
        ../../bcachefs.nix
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "e67c05fc";
    };
}
