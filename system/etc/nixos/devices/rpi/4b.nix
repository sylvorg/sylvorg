{ config, lib, pkgs, type, ... }: with builtins; with lib; with j; mkIf (type == "4b") {
    vars.terminal = true;
    boot = {
        kernelParams = ["cma=256M"];
        loader = {
            raspberryPi = {
                enable = true;
                version = 4;
                uboot.enable = true;
                firmwareConfig = '' gpu_mem=256 '';
            };
        };
    };
    environment.systemPackages = with pkgs; [ raspberrypi-tools ];
}
