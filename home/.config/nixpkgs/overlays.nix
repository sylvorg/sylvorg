with builtins;
    with (getFlake "/etc/nixos");
    with lib;
    with j;
let stc = legacyPackages.hostName.${getEnv "HOSTNAME"};
in getAttrFromPath (attrValues stc) legacyPackages.overlays
