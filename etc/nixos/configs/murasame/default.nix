{ config, ... }:

{
    imports = [
        ../../super.nix
        ../../bcachefs.nix
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "ba10757c";
    };
}
