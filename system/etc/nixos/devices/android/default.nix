{ config, lib, ... }: with builtins; with lib; with j; {
    imports = imprelib.list { dir = ./.; };
    config.vars = { terminal = true; minimal = true; };
}
