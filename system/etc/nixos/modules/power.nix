{ config, lib, ... }: with builtins; with lib; with j; {
    services.logind.lidSwitch = "hybrid-sleep";
    powerManagement = {
        enable = true;
        cpuFreqGovernor = mkForce "ondemand";
    };
}
