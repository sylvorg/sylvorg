{ config, lib, pkgs, system, host, ... }: with lib;

{
    services = {
        emacs = {
            package = pkgs.emacsGcc;
            enable = true;
            defaultEditor = true;
        };
        # flatpak.enable = j.attrs.no-arms;
        flatpak.enable = true;
        guix.enable = true;
        printing.enable = true;
    };
}
