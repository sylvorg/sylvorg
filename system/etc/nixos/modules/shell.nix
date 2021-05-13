{ config, lib, ... }: with builtins; with lib; with j; {
    programs = {
        xonsh.enable = true;
        fish.enable = true;
        zsh.enable = true;
    };
}
