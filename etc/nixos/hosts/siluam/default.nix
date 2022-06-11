{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "7fe26f3d";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
