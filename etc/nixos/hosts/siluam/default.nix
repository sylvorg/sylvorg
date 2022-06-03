{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "0bb3af87";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
