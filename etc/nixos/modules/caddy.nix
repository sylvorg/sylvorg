{ config, lib, ... }: with builtins; with lib;

{
    services.caddy = mkIf config.variables.relay {
        enable = true;
        ca = null;
        config = readFile "/etc/caddy/files/${config.networking.hostName}";
        adapter = "yaml";
    };
}
