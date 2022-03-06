{ config, pkgs, lib, ... }:

{
    imports = import ../imports.nix;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "a91dc84d";
    };
    boot = {
        kernelPackages = lib.mkForce pkgs.linuxPackages_rpi4;
        initrd.availableKernelModules = [ "usbhid" "usb_storage" ];
        kernelParams = [
            "8250.nr_uarts=1"
            "console=ttyAMA0,115200"
            "console=tty1"
            "cma=128M"
        ];
        loader = {
            raspberryPi = {
                enable = true;
                version = 4;
            };
            grub.enable = false;
            generic-extlinux-compatible.enable = true;
        };
    };
    hardware.enableRedistributableFirmware = true;
}
