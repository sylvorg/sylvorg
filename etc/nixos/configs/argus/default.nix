{ config, pkgs, lib, ... }:

{
    imports = import ../imports.nix;
    boot = {
        kernelPackages = lib.mkForce pkgs.linuxPackages_rpi4;
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
            grub = {
                enable = false;
                extraConfig = ''
                    serial --speed=19200 --unit=0 --word=8 --parity=no --stop=1;
                    terminal_input serial;
                    terminal_output serial;
                '';
            };
            generic-extlinux-compatible.enable = true;
        };
        initrd.availableKernelModules = [ "usbhid" "usb_storage" ];
    };
    networking = {
        usePredictableInterfaceNames = false;
        interfaces.eth0.useDHCP = true;
        hostName = baseNameOf (toString ./.);
        hostId = "ac26a54a";
    };
}
