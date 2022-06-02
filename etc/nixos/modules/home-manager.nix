{ config, lib, ... }: with lib;

{
    home-manager = {
        useGlobalPkgs = true;
        users = j.foldToSet [
            listToAttrs (map (user: nameValuePair user {
                home = {
                    homeDirectory = j.attrs.homes.${user};
                };
            }) j.attrs.allUsernames)
        ];
    };
}
