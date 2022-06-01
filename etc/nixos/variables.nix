{ options, ... }:

{
    options.variables = mkOption {
        type = types.attrs;
        default = { };
    }
}
