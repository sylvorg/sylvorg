{ config, ... }:

{
    imports = [
        ../../super.nix

        # TODO
        ../../bcachefs.nix

    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "e4bef6cf";
    };
}
