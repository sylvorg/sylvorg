{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "ea302844";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
