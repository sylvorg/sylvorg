{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "466bb8a6";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
