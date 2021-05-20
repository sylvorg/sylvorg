{ config, lib, pkgs, ... }: with builtins; with lib; with j; {
    boot = {
        # kernelPackages = pkgs.linuxPackages_latest_xen_dom0_hardened;
        # kernelPackages = pkgs."linuxPackages_xanmod_v5.10.4_cacule";
        kernelPackages = pkgs.linuxPackages_lqx;
        # kernelPackages = pkgs.linuxPackages_zen;
        kernelPatches = [
            # { name = "clear"; patch = ./patches/0110-initialize-ata-before-graphics.patch; }
            {
                name = "Enable ZSTD Compression";
                patch = null;
                extraConfig = ''
                    RD_ZSTD y
                    KERNEL_ZSTD y
                    KERNEL_XZ n
                '';
            }
        ];
        # extraModulePackages = with config.boot.kernelPackages; [ anbox wireguard ];
    };
}
