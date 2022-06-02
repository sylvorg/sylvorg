{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "9c96a0cc";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
