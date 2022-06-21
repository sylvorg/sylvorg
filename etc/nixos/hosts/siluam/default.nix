{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "4a4fb868";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
