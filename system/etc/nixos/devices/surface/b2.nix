{ config, lib, device, ... }: with builtins; with lib; with j; mkIf (device == "b2") {
    boot.extraModprobeConfig = mkAfter '' options ipts singletouch=y '';
    powerManagement.resumeCommands = mkAfter '' modprobe ipts_surface singletouch=y '';
    services.surface-dtx-daemon.detach = mkForce ''
        #!/usr/bin/env xonsh
        for usb in $(ls /dev/disk/by-id).split("\n"):
        if usb and usb[:4] == "usb-":
            for mnt in $(mount).split("\n"):
                if mnt and usb in mnt:
                    umount @(mnt.split()[2])
    '';
}
