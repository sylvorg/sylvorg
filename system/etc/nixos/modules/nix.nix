{ config, lib, ... }: with builtins; with lib; with j; {
    nix = rec {
        gc = foldToSet [
            { automatic = true; }
            { dates = "monthly"; }
            # {
            #   dates = "monthly";
            #   options = "-d";
            # }
            # {
            #   dates = "daily";
            #   options = "--delete-older-than 30d";
            # }
        ];
        optimise = {
            automatic = true;
            dates = [ "05:00" ];
        };
        autoOptimiseStore = true;
        extraOptions = attrs.configs.nix;
        useSandbox = true;
        binaryCaches = flatten [
            (map (dir: "file:///${dir}/") [
                "chimchar"
                "empoleon"
            ])
        ];
        # sandboxPaths = [];
    };
}
