{
    description = "";
    inputs = {
        shadowrylander = {
            url = "github:shadowrylander/shadowrylander/master";
            flake = false;
        };
        wip-pinebook-pro = {
            url = "github:shadowrylander/wip-pinebook-pro/master";
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
