{ config, lib, pkgs, device, ... }: with builtins; with lib; with j; mkIf (device == "4b") {
    vars.terminal = true;
    boot = {
        kernelPackages = mkOverride 49 pkgs.linuxPackages_rpi4;
        tmpOnTmpfs = true;
        initrd.availableKernelModules = [ "usbhid" "usb_storage" ];
        # ttyAMA0 is the serial console broken out to the GPIO
        kernelParams = [
            "8250.nr_uarts=1"
            "console=ttyAMA0,115200"
            "console=tty1"
            # Some gui programs need this
            "cma=128M"
        ];
        loader = {
            raspberryPi = {
                enable = true;
                version = 4;
                firmwareConfig = ''
                    dtparam=sd_poll_once=on
                    dtparam=audio=on
                '';
            };
            grub.enable = mkForce false;
            systemd-boot.enable = mkForce false;
            generic-extlinux-compatible.enable = true;
            
        };
    };

    services.xserver.videoDrivers = [ "fbdev" ];    
    hardware = {
        enableRedistributableFirmware = true;
        raspberry-pi."4".fkms-3d.enable = true;
        pulseaudio.enable = true;
    };
    sound.enable = true;
    environment.systemPackages = with pkgs; [ raspberrypi-tools libraspberrypi ];
}
