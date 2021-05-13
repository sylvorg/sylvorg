{ sources, ... }: with builtins; rec {
    nixos = toString (./. + "/..");
    lib = "${nixos}/lib";
    extras = "${nixos}/extras";
    configs = "${nixos}/configs";
    modules.flakes = "${nixos}/flake_modules";
    global = "${nixos}/global";
    patches = {
        _  = "${nixos}/patches";
        surface = "${sources.nixos-surface}/linux-surface/patches";
    };
}
