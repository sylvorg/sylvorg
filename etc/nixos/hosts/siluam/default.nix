{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "3b70ee93";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
