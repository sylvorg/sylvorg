{
    description = "Overlay Sources";

    inputs = {
        flake-compat = {
            url = "github:edolstra/flake-compat/master";
            flake = false;
        };
        emacs = {
            url = "github:nix-community/emacs-overlay/master";
            flake = false;
        };
        nur.url = "github:nix-community/nur/master";
        mach-nix.url = "github:davhau/mach-nix/master";
        mozilla = {
            url = "github:mozilla/nixpkgs-mozilla/master";
            flake = false;
        };
        nanite = {
            url = "git+https://gitlab.com/picotech/nanotech/nanite.git";
            flake = false;
        };
        niv = {
            url = "github:nmattia/niv/master";
            flake = false;
        };
        nix.url = "github:nixos/nix/master";
        qtile = {
            url = "github:qtile/qtile/master";
            flake = false;
        };
        xonsh = {
            url = "github:xonsh/xonsh/main";
            flake = false;
        };
        wip-pinebook-pro = {
            url = "github:shadowrylander/wip-pinebook-pro/master";
            flake = false;
        };
        nixpkgs.url = "github:nixos/nixpkgs/master";
        nixpkgs-20-03.url = "github:nixos/nixpkgs/nixos-20.03";
        nixpkgs-20-03-small.url = "github:nixos/nixpkgs/nixos-20.03-small";
        nixpkgs-20-09.url = "github:nixos/nixpkgs/nixos-20.09";
        nixpkgs-20-09-small.url = "github:nixos/nixpkgs/nixos-20.09-small";
        nixpkgs-21-05.url = "github:nixos/nixpkgs/nixos-21.05";
        nixpkgs-21-05-small.url = "github:nixos/nixpkgs/nixos-21.05-small";
        nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
        nixpkgs-unstable-small.url = "github:nixos/nixpkgs/nixos-unstable-small";
        nixpkgs-master.url = "github:nixos/nixpkgs/master";
    };

    outputs = inputs@{ self, flake-compat, ... } : {  };
}
