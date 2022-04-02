{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "ab0b9189";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
