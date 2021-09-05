{
    description = "";
    inputs = {
         = {
            url = "github://master";
            flake = false;
        };
        wip-pinebook-pro = {
            url = "github:/wip-pinebook-pro/master";
            flake = false;
        };
        home-manager' = {
            url = "github:nix-community/home-manager";
            flake = false;
        };
        impermanence = {
            url = "github:nix-community/impermanence";
            flake = false;
        };
    };
    outputs = { self, ... }: {  };
}
