{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "33d0e4d5";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
