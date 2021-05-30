{ config, lib, system, ... }: with builtins; with lib; with j; {
    programs = {
        xonsh.enable = true;
        fish = mkIf (!elem system [ "aarch64-linux" ]) {
            enable = true;
        };
        zsh.enable = true;
    };
}
