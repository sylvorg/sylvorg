{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "c4c9f9a7";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
