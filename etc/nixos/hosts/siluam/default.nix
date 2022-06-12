{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "0228bb75";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
