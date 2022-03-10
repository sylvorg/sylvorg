{ config, pkgs, lib, ... }:

{
    imports =  [
        ../../minimal.nix
        (import ./.).inputs.hardware.raspberry-pi-4
    ];
    boot.kernelPackages = lib.mkForce pkgs.linuxPackages_rpi4;
}
