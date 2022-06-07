{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "ff85d8cd";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
