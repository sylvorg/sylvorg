{ config, pkgs, lib, ... }:

{
    imports =  [
        ../minimal.nix
        ../profiles/server.nix
    ];
    hardware.enableRedistributableFirmware = true;
    networking.wireless.enable = true;
    sound.enable = true;
    hardware.pulseaudio.enable = mkForce true;
    boot.loader.raspberryPi.firmwareConfig = ''
        dtparam=audio=on
    '';
    boot.kernelParams = [
        "console=ttyS1,115200n8"
    ];
    boot.loader.raspberryPi = {
        enable = true;
        version = 3;
        firmwareConfig = ''
            core_freq=250
        '';
    };
    systemd.services.btattach = {
        before = [ "bluetooth.service" ];
        after = [ "dev-ttyAMA0.device" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
            ExecStart = "${pkgs.bluez}/bin/btattach -B /dev/ttyAMA0 -P bcm -S 3000000";
        };
    };
    boot.loader.raspberryPi.enable = true;
    # Set the version depending on your raspberry pi. 
    boot.loader.raspberryPi.version = 3;
    # We need uboot
    boot.loader.raspberryPi.uboot.enable = true;
    # These two parameters are the important ones to get the
    # camera working. These will be appended to /boot/config.txt.
    boot.loader.raspberryPi.firmwareConfig = ''
        start_x=1
        gpu_mem=256
    '';
    boot.kernelModules = [ "bcm2835-v4l2" ];
    boot.initrd.kernelModules = [ "vc4" "bcm2835_dma" "i2c_bcm2835" ];
}
