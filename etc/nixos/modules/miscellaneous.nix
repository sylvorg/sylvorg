{ config, pkgs, lib, system, inputs, ... }: with lib;

{
    xdg.portal = {
        enable = mkForce j.attrs.no-arms;
        extraPortals = map (portal: pkgs."xdg-desktop-portal-${portal}") [ "gtk" "kde" ];
    };
    i18n = {
        # Select internationalisation properties.
        defaultLocale = "en_US.UTF-8";
    };
    time.timeZone = "America/Toronto";
    system = {
        inherit (nixos-configurations.configuration.config.system) stateVersion;
        autoUpgrade = {
            enable = true;
            allowReboot = false;
            flake = "https://github.com/nixos/nixpkgs/archive/${inputs.nixpkgs.rev}.tar.gz";
        };
    };
}
