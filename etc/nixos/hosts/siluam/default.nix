{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "f160c0c5";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
