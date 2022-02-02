{ config, ... }:

{
    imports = [
        ../../super.nix

        # TODO
        ../../bcachefs.nix

    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "9e7088cd";
    };
}
