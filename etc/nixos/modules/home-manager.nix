{ config, lib, ... }: with lib;

{
    home-manager = {
        useGlobalPkgs = true;
        users = j.functions.foldToSet [
            listToAttrs (user: nameValuePair user {
                home = {
                    homeDirectory = j.attrs.homes.${user};
                    file.dross.source = repo;
                };
            }) j.attrs.allUsers
        ];
    };
}
