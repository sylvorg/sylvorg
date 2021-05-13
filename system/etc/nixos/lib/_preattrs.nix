lib: with builtins; with lib; rec {
    integer-defaults = {
        noSwap = 0;
        musl = 1;
        minimal = 0;
        terminal = 0;
        zfs = 1;
    };
    default-stc = integer-defaults // {
        system = currentSystem;
        type = "def";
        device = "def";
        channel = "pkgs";
    };
    stc = attrNames default-stc;
    home-manager-integer-defaults = { nixos = 1; };
    home-manager-default-stc = default-stc
        // home-manager-integer-defaults
        // { user = "root"; };
    home-manager-stc = attrNames home-manager-default-stc;
}
