{ config, lib, pkgs, ... }: with builtins; with lib; with j; {
    virtualisation = lib.mkIf (!config.vars.minimal) {
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
