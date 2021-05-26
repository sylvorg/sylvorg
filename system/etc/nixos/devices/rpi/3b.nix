{ config, lib, pkgs, type, ... }: with builtins; with lib; with j; mkIf (type == "3b") {
    vars.terminal = true;
    boot = {
        kernelParams = ["cma=256M"];
        loader = {
            raspberryPi = {
                enable = true;
                version = 3;
                uboot.enable = mkForce true;
                firmwareConfig = '' gpu_mem=256 '';
            };
        };
    };
    environment.systemPackages = with pkgs; [ raspberrypi-tools libraspberrypi ];
}
