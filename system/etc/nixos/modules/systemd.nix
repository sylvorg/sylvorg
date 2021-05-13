{ config, lib, pkgs, ... }: with builtins; with lib; with j; {
    systemd = {
        packages = with pkgs; [ runit ly ];
        services = {
            runit.enable = true;
            ly.enable = true;
        };
    };
}
