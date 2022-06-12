{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "ebc9ed2e";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
