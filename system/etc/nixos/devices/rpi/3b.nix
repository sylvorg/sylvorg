{ config, lib, pkgs, device, ... }: with builtins; with lib; with j; mkIf (device == "3b") {
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
    environment.systemPackages = with pkgs; [ libraspberrypi ];
}
