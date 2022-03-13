{ config, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        (import ./.).inputs.hardware.microsoft-surface
    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "/home/shadowrylander/.zshenv:source:1: no such file or directory: /home/shadowrylander/resources/grml/etc/zsh/zshenv
        hostId =  9686ea8f";
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
