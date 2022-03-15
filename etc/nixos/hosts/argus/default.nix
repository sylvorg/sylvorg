{ config, pkgs, lib, ... }:

{
    imports = import ../imports.nix;
    boot = {
        kernelParams = [ "console=ttyS0,19200n8" ];
        loader.grub.extraConfig = ''
            serial --speed=19200 --unit=0 --word=8 --parity=no --stop=1;
            terminal_input serial;
            terminal_output serial;
        '';
    };
    networking = {
        usePredictableInterfaceNames = false;
        interfaces.eth0.useDHCP = true;
        hostName = baseNameOf (toString ./.);
        hostId = "/home/shadowrylander/.zshenv:source:1: no such file or directory: /home/shadowrylander/resources/grml/etc/zsh/zshenv
        hostId =  0a0dddb2";
    };
}
