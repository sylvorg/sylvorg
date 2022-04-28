{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "3a0f8d9b";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
