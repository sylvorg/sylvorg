{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "1d1862c0";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
