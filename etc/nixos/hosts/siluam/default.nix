{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        "${(import ../..).inputs.pinebook-pro}/pinebook_pro.nix"
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "bd8dcc74";
    };
    swapDevices = [ { device = "/dev/mmcblk2p2"; } ];
}
