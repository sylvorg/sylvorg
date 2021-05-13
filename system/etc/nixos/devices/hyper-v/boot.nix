{ config, lib, pkgs, ... }: with builtins; with lib; with j; {
    boot = {
        # REQUIRED - see: https://github.com/nixos/nixpkgs/issues/9899

        kernelModules = [ "hv_vmbus" "hv_storvsc" ];
        initrd = {
            kernelModules = [ "hv_vmbus" "hv_storvsc" ];
            availableKernelModules = [ "hv_vmbus" "hv_storvsc" ];

            # UNKNOWN - not sure if below are needed; were suggested for VirtualBox and I used them
            checkJournalingFS = false;

        };
        extraModulePackages = with pkgs.linuxPackages_latest; [ hyperv-daemons ];

        # RECOMMENDED
        # - use 800x600 resolution for text console, to make it easy to fit on screen
        kernelParams = ["video=hyperv_fb:800x600"];  # https://askubuntu.com/a/399960
        # - avoid a problem with `nix-env -i` running out of memory
        # kernel.sysctl."vm.overcommit_memory" = "1"; # https://github.com/NixOS/nix/issues/421
    };
}
