{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "fd3a7aec";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
