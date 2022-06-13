{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "c1eb074d";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
