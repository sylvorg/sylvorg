{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        (import ./.).hardware.microsoft-surface
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "ae8ae10e";
        networkmanager.extraConfig = ''
            [connection]
            wifi.powersave = 2

            [device]
            wifi.scan-rand-mac-address=false
        '';
    };
    services.surface-dtx-daemon.detach = mkForce ''
        #!/usr/bin/env sh
        for usb in $(ls /dev/disk/by-id).split("\n"):
            if usb and usb[:4] == "usb-":
                for mnt in $(mount).split("\n"):
                    if mnt and usb in mnt:
                        umount @(mnt.split()[2])
    '';
}
