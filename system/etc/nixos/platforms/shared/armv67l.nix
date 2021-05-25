{ config, lib, ... }: with builtins; with lib; with j; {
    nix = {
        binaryCaches = [
            "http://nixos-arm.dezgeg.me/channel"
            "https://app.cachix.org/cache/thefloweringash-armv7"
        ];
        binaryCachePublicKeys = [
            "nixos-arm.dezgeg.me-1:xBaUKS3n17BZPKeyxL4JfbTqECsT+ysbDJz29kLFRW0=%"
            "thefloweringash-armv7.cachix.org-1:v+5yzBD2odFKeXbmC+OPWVqx4WVoIVO6UXgnSAWFtso="
        ];
    };
}
