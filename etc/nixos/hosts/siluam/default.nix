{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ./.).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "2abc954a";
    };
    swapDevices = [ "/dev/mmcblk2p2" ];
}
