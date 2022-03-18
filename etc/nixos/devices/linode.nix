{ config, pkgs, lib, ... }:

{
    boot = {
        kernelParams = [ "console=ttyS0,19200n8" ];
        loader.grub.extraConfig = ''
            serial --speed=19200 --unit=0 --word=8 --parity=no --stop=1;
            terminal_input serial;
            terminal_output serial;
        '';
        initrd.availableKernelModules = [ "virtio_pci" "ahci" "sd_mod" ];
    };
    networking = {
        usePredictableInterfaceNames = false;
        interfaces.eth0.useDHCP = true;
    };
    services = {
        openssh = {
            enable = true;
            extraConfig = mkOrder 0 ''
                TCPKeepAlive yes
                ClientAliveCountMax 480
                ClientAliveInterval 3m
            '';
            permitRootLogin = "yes";
        };
    };
    environment.systemPackages = with pkgs; [ inetutils mtr sysstat git ];
}
