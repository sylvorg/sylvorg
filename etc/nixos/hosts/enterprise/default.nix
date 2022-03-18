{ config, ... }:

{
    imports = [ ../../devices/rpi4.nix ];
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "/home/shadowrylander/.zshenv:2: parse error near `\n'
        hostId =  9582a8fa";
    };
}
