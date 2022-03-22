{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ./.).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "68292714";
    };
    swapDevices = [ "/dev/mmcblk2p2" ];
}
