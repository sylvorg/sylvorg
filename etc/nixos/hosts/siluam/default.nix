{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "29d23f8c";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
