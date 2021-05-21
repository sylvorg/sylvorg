{ config, pkgs, lib, system, ... }: with builtins; with lib; with j; let b = "bcachefs"; in mkIf (
    !elem system [ "aarch64-linux" ]
) {
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
