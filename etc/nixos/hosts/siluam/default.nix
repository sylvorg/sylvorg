{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "7d9fbc78";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
