{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ./.).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "d820fca6";
    };
    swapDevices = [ "/dev/mmcblk2p2" ];
}
