{ config, ... }:

{
    imports = import ../imports.nix;
    networking = {
        hostName = baseNameOf (toString ./.);
        hostId = "/home/shadowrylander/.zshenv:2: parse error near `\n'
        hostId =  778feecb";
    };
}
