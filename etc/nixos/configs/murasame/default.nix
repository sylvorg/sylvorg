{ config, ... }:

{
    imports = [
        ../../super.nix

        # TODO
        # ../../bcachefs.nix

    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "6effad58";
    };
}
