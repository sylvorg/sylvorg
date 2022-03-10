{ config, lib, pkgs, ... }: with lib;

{
    systemd = {
        # packages = with pkgs; [ runit ];
        services = {
            # runit.enable = true;
            caddy = mkIf (elem config.networking.hostName j.attrs.machines.relays) (j.attrs.configs.services.base // {
                serviceConfig = {
                    ExecStart = ''
                        ${pkgs.caddy}/bin/caddy run --config ${j.attrs.homes.${j.attrs.users.primary}}/.config/caddy/files/${config.networking.hostName} --adapter yaml 2>&1
                    '';
                    ExecStop = ''
                        pkill caddy
                    '';
                };
            });
        };
    };
}
