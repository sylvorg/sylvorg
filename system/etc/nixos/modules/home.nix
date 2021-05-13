{ config, lib, ... }: with builtins; with lib; with j; {
    # home-manager.users = let
    #     stc = {}
    #     inherit (config.vars) nixos zfs;
    # in listToAttrs (map (user: nameValuePair user (import /root/.config/nixpkgs/home.nix nixos zfs stc)) attrs.allUsers);
}
