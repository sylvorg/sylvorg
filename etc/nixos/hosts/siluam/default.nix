{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "3ff1255a";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
