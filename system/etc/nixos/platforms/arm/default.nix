{ config, lib, ... }: with builtins; with lib; with j; {
    imports = imprelib.list { dir = ./.; };
    config = {
        # NixOS wants to enable GRUB by default
        boot.loader.grub.enable = false;
        # Enables the generation of /boot/extlinux/extlinux.conf
        boot.loader.generic-extlinux-compatible.enable = true;
    };
}
