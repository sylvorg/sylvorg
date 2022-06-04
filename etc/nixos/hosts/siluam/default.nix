{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "4e3a8125";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
