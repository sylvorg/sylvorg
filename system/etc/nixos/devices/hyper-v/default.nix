{ config, lib, ... }: with builtins; with lib; with j; {
    imports = imprelib.list { dir = ./.; };
    config = {
        vars.noSwap = true;
        networking = {
            wireless.enable = false;
            interfaces.eth0.useDHCP = true;
        };
        virtualisation.hypervGuest.enable = true;
    };
}
