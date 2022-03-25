{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "5bda3bb3";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
