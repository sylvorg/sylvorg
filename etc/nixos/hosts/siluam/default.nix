{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "8912f9d9";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
