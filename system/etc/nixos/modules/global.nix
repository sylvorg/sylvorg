{ config, lib, ... }: {
    nixpkgs = nixpkgset;
    i18n = {
        # Select internationalisation properties.
        defaultLocale = "en_US.UTF-8";
    };
    time.timeZone = "America/Toronto";
    system = {
        # This value determines the NixOS release from which the default
        # settings for stateful data, like file locations and database versions
        # on your system were taken. It‘s perfectly fine and recommended to leave
        # this value at the release version of the first install of this system.
        # Before changing this value read the documentation for this option
        # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
        # stateVersion = "20.09"; # Did you read the comment?
        autoUpgrade = {
            enable = true;
            allowReboot = false;
            flake = https://github.com/nixos/nixpkgs/archive/master.tar.gz;
        };
    };
    boot.binfmt.emulatedSystems = [
        "armv7l-linux"
        "aarch64-linux"
    ];

    # Flatpak
    xdg.portal.enable = true;

    environment.pathsToLink = [ "/share/nix-direnv" ];
    zramSwap = {
        enable = true;
        algorithm = "zstd";
    };
}
