{ config, pkgs, ... }: with pkgs;

{
    boot = {
        binfmt.emulatedSystems = [
            "armv7l-linux"
            "aarch64-linux"
        ];
        extraModulePackages = with config.boot.kernelPackages; [
            # anbox
            # wireguard
        ];
    };
}
