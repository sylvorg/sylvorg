{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "a1bb006b";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
