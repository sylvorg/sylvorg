{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "9c42e73c";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
