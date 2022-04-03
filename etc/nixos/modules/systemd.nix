{ config, lib, pkgs, host, ... }: with lib;

{
    systemd = {
        # packages = with pkgs; [ runit ];
        services = {
            # runit.enable = true;
            caddy = mkIf j.attrs.relay (j.attrs.configs.services.base // {
                serviceConfig = {
                    ExecStart = ''
                        ${pkgs.caddy}/bin/caddy run --config ${j.attrs.homes.${j.attrs.users.primary}}/.config/caddy/files/${host} --adapter yaml 2>&1
                    '';
                    ExecStop = ''
                        pkill caddy
                    '';
                };
            });
        };
    };
}
