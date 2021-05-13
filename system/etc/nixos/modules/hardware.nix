{ config, lib, ... }: with builtins; with lib; with j; {
    hardware = {
        enableRedistributableFirmware = lib.mkDefault true;
        # Enable sound
        pulseaudio.enable = true;
    };
    sound.enable = true;
}
