{ config, ... }:

{
    imports = [
        ../../super.nix

        # TODO
        ../../bcachefs.nix

    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "fe97057d";
    };
}
