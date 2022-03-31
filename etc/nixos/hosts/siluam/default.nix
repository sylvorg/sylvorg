{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "49abdf1b";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
