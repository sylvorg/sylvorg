{ config, ... }:

{
    imports = [
        ../../super.nix

        # TODO
        ../../bcachefs.nix

    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "0070ca6d";
    };
}
