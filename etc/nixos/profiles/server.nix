{ config, pkgs, lib, ... }: with lib;

{
    services.openssh = {
        enable = true;

        # TODO: Doesn't work on arm, for some reason
        # allowSFTP = true;
        allowSFTP = false;

        extraConfig = mkOrder 0 ''
            TCPKeepAlive yes
            ClientAliveCountMax 480
            ClientAliveInterval 3m
        '';
        permitRootLogin = "yes";
    };

    environment.systemPackages = with pkgs; [ inetutils mtr sysstat git ];
}
