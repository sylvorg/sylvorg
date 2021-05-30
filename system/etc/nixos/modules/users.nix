inputs@{ config, lib, pkgs, sources, stc, system, ... }: with builtins; with lib; with j; {
    users = with attrs.users; let
        base = mkMerge [{
                hashedPassword = "$6$DoC/h6kR66Sa$aZKtTOXAqnan/jAC.4dH9tCYshheiKUZItR4g/kmMMLsfLQh0KslINL9zUTX2IjAZh9DE18eAh1AAz48.n/cm.";
                isNormalUser = true;
                createHome = true;
                extraGroups = [
                    "wheel"
                    "networkmanager"
                    "persist"
                ];
                openssh.authorizedKeys.keys = [
                    attrs.ssh.keys.master
                ];
                packages = import (
                    if (pathExists ../packages.nix) then ../packages.nix else ./packages.nix
                ) inputs;
            }
            (mkIf (!config.vars.minimal) {
                extraGroups = [ "libvirtd" "docker" ];
            })
        ];
    in rec {
        users = mkMerge [
            (genAttrs attrs.allUsers (user: base))
            {
                "${primary}" = {
                    uid = 4362;
                    home = attrs.allHomes.${primary};
                    description = "Jeet Ray";
                    group = primary;
                    extraGroups = [ secondary ];
                    shell = pkgs.xonsh;
                };
                "${secondary}" = {
                    uid = 1111;
                    home = attrs.allHomes.${secondary};
                    description = "Alicia Summers";
                    group = secondary;
                    extraGroups = [ primary ];
                    shell = if (!elem system [ "aarch64-linux" ]) then pkgs.fish else pkgs.zsh;
                };
                "${nightingale}" = {
                    uid = 8888;
                    home = attrs.allHomes.${nightingale};
                    description = "Curtis Nightingale";
                    group = "root";
                    extraGroups = [ primary secondary ];
                    shell = pkgs.zsh;
                };
                root = {
                    shell = mkForce pkgs.xonsh;
                    home = attrs.allHomes.root;
                    isNormalUser = mkForce false;
                    isSystemUser = mkForce true;
                };
            }
        ];

        mutableUsers = false;

        groups = {
            "${primary}" = {
                gid = config.users.users.${primary}.uid;
                members = [ primary secondary nightingale ];
            };
            "${secondary}" = {
                gid = config.users.users.${secondary}.uid;
                members = [ primary secondary nightingale ];
            };
            "${nightingale}" = {
                gid = config.users.users.${nightingale}.uid;
                members = [ nightingale ];
            };
        };
    };
}
