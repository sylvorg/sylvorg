{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "fe9a6864";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
