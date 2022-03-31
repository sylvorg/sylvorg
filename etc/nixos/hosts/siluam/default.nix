{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "f0b143d1";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
