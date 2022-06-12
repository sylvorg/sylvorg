{ config, ... }:

{
    imports = [ ../../minimal.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "5f50f592";
    };
}
