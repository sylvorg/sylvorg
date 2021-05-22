{ config, lib, pkgs, ... }: with builtins; with lib; with j; mkIf (
    !(config.vars.minimal || elem system [ "aarch64-linux" ])
) {
    virtualisation = {
        xen.enable = false;
        lxd = { zfsSupport = true; };
        podman.enable = true;
        docker = {
            enable = true;
            storageDriver = "zfs";
            package = pkgs.docker;
            enableOnBoot = true;
        };
        libvirtd.enable = true;
    };
}
