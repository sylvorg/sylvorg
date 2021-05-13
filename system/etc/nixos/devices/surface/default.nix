{ config, lib, ... }: with builtins; with lib; with j; {
    imports = imprelib.list { dir = ./.; };
    config = {
        boot.kernelPackages = pkgs.surface_kernel_latest;
        networking.networkmanager.extraConfig = ''
            [connection]
            wifi.powersave = 2

            [device]
            wifi.scan-rand-mac-address=false
        '';
    };
}
