{ config, options, lib, pkgs, ... }: with lib; {
    services.openssh = {
        enable = true;
        allowSFTP = false;
        hostKeys = options.services.openssh.hostKeys.default;
        extraConfig = mkOrder 0 ''
            TCPKeepAlive yes
            ClientAliveCountMax 480
            ClientAliveInterval 3m
        '';
        permitRootLogin = "yes";
    };

    environment.systemPackages = with pkgs; [ inetutils mtr sysstat git ];
}
