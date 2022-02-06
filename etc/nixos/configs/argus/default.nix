{ config, ... }:

{
    imports = [
        ../../super.nix
        ../../bcachefs.nix
    ];
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
        hostId = "13ab5839";
    };
}
