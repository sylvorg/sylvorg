{ config, lib, stc, ... }: with builtins; with lib; with j; {
    options = {
        vars = mkOption {
            default = mkDefault {  };
            type = with lib.types; attrsOf bool;
        };
    };
    config.vars = {
        bootPart = mkDefault true;
        syncDevice = mkDefault false;
    } // (mapAttrs (
        n: v: mkDefault (if (isInt v) then (v == 1) else v)
    ) (default-stc // stc));
}
