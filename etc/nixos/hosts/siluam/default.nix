{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "d81b6bd3";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
