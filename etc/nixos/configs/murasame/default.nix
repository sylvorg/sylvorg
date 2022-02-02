{ config, ... }:

{
    imports = [
        ../../super.nix

        # TODO
        # ../../bcachefs.nix

    ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "2893df96";
    };
}
