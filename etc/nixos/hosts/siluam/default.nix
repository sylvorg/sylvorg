{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "77c44dd6";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
