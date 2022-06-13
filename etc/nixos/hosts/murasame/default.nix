{ config, ... }:

{
    imports = [ ../../minimal.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "c36b84cf";
    };
}
