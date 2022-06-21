{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "21af3f4e";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
