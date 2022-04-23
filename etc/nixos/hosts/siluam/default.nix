{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "cb1f5352";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
