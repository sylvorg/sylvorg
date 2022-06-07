{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "9b36fad4";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
