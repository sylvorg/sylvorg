{ config, ... }:

{
    imports = [
        ../../super.nix

        # TODO
        # ../../bcachefs.nix

    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "782f9e40";
    };
}
