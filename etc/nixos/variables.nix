{ config, options, lib, ... }: with lib;

{
    options.variables = {
        zfs = mkOption {
            type = types.bool;
            default = true;
        };
        relay = mkOption {
            type = types.bool;
            default = false;
        };
        server = mkOption {
            type = types.bool;
            default = config.variables.relay;
        };
        client = mkOption {
            type = types.bool;
            default = (! config.variables.server) && (! config.variables.relay);
        };
        minimal = mkOption {
            type = types.bool;
            default = false;
        };
    };
    config._module.args.variables = config.variables;
}
