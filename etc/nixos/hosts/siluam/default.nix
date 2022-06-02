{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "e6594b7e";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
