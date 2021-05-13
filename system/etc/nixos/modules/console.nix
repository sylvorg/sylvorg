{ config, lib, ... }: with builtins; with lib; with j; {
    console = {
        # Select internationalisation properties.
        # font = lib.mkDefault "${terminus_font}/share/consolefonts/ter-u28n.psf.gz";
        font = "Cartograph CF Light Italic";
        keyMap = "us";
    };
}
