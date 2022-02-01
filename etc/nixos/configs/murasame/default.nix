{ config, ... }:

{
    imports = [
        ../../super.nix
        ../../bcachefs.nix
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "92b64749";
    };
}
