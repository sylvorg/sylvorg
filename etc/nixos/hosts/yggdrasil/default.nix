{ config, ... }:

{
    imports = [ ../rpi4.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "8e6c54cd";
    };
}
