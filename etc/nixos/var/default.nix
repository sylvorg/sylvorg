{ config, lib, ... }: {
    imports = [ ./var.nix ];
    config.system.activationScripts.vars = lib.stringAfter [ "users" "groups" ] config.system.build.varActivationCommands;
}
