{ config, pkgs, lib, ... }: with builtins; with lib; with j; let b = "bcachefs"; in {
    boot = {
        kernelPackages = mkForce pkgs."linuxPackages_testing_${b}";
        kernelModules = [ b ];
        initrd = {
            kernelModules = [ b ];
            availableKernelModules = [ b ];
        };
        supportedFilesystems = [ "bcachefs" ];
    };
}
