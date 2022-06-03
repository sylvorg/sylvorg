{ config, lib, ... }: with lib;

{
    home-manager = {
        users = mapAttrs' (designation: user: nameValuePair user {}) j.attrs.allUsers;
    };
}
