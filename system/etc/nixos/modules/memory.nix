{ config, lib, host, ... }: with builtins; with lib; with j; mkIf (
    config.vars.minimal -> config.vars.noSwap
) { swapDevices = [{ device = "/dev/zvol/${host}/swap"; }]; }
