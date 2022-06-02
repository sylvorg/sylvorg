{ config, pkgs, lib, ... }: let
    relayNo = if config.variables.relay then "no" else "yes";
    relayYes = if config.variables.relay then "yes" else "no";
in {
    imports = [ ../variables.nix ];
    services.openssh = {
        enable = true;
        extraConfig = lib.mkOrder 0 ''
            TCPKeepAlive yes
            ClientAliveCountMax 480
            ClientAliveInterval 3m
        '';
        permitRootLogin = "yes";
        openFirewall = config.variables.relay;
    };
    environment.systemPackages = with pkgs; [ inetutils mtr sysstat git ];
    variables.server = true;
}
