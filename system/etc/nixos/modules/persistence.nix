{ config, lib, ... }: with builtins; with lib; with j; {
    environment.${myIf.knull config.vars.zfs "persistence"} = attrs.persistence.system {};
}
