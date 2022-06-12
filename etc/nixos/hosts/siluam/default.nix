{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "fcd9916d";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
