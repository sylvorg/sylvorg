{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "e2055ee1";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
