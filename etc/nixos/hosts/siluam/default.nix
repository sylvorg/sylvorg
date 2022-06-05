{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "b2f68a71";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
