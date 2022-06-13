{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "a8817f2e";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
