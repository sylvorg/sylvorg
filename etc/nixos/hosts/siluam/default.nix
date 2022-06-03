{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "d944e1fa";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
