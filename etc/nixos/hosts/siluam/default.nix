{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ./.).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "3c4f631e";
    };
}
