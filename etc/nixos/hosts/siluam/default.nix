{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "d2be1d60";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
