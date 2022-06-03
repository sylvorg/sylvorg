with builtins; args@{ config, ... }: let
    flake = import ./.;
    system = args.system or currentSystem;
    host = args.host or config.networking.hostName;
    inheritanceSet = if (args ? inputs) then args else (flake.make.specialArgs host system);
    inherit (inheritanceSet) lib overlays nixpkgset pkgs;
in with lib; {
    boot = {
        supportedFilesystems = j.attrs.fileSystems.supported;
        initrd = {
            inherit (config.boot) supportedFilesystems;
            compressor = "${lib.getBin pkgs.zstd}/bin/zstd";
        };
        kernelPackages = mkDefault pkgs.linuxPackages_xanmod;
        # kernelPackages = mkDefault pkgs.linuxPackages_lqx;
        # kernelPackages = mkDefault pkgs.linuxPackages_zen;
        kernelPatches = flatten [
            (optionals (elem "bcachefs" config.boot.supportedFilesystems) (filter (set: hasInfix "bcachefs" set.name) pkgs.linuxKernel.kernels.linux_testing_bcachefs.kernelPatches))
        ];
    };
}
