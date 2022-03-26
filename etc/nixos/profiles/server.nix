args@{ config, ... }: with builtins; let
    system = args.system or currentSystem;
    host = args.host or config.networking.hostName;
    flake = import ./..;
    inheritanceSet = flake.make.specialArgs host system;
    inherit (inheritanceSet) lib pkgs;
in with lib; {
    services.openssh = {
        enable = true;

        # TODO: Doesn't work on arm, for some reason
        allowSFTP = j.attrs.no-arms;

        extraConfig = mkOrder 0 ''
            TCPKeepAlive yes
            ClientAliveCountMax 480
            ClientAliveInterval 3m
        '';
        permitRootLogin = "yes";
    };

    environment.systemPackages = with pkgs; [ inetutils mtr sysstat git ];
}
