args@{ config, lib, pkgs, ... }: with lib; {
    services.openssh = {
        enable = true;
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
