{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "4d41b8cc";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
