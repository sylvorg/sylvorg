lib: nixpkgs: pkgs: channel: with builtins; with lib;
let
channels = (map (d: "nixos-" + d) (
    "unstable"
    "unstable-small"
    "21.11"
    "21.11-small"
)) + (map (d: "release-" + d) (
    "21.11"
)) + (
    "master"
);
othernixpkgs = listToAttrs (map (ref: nameValuePair
    ref
    (fetchGit { url = "https://github.com/nixos/nixpkgs"; inherit ref; })
) channels);
otherpkgs = mapAttrs (n: v: import v j.attrs.configs.nixpkgs) othernixpkgs;
in flatten [
[(final: prev: { guix = final.callPackage "${fetchGit { url = "https://github.com/${j.attrs.users.primary}/nixpkgs"; ref = "guix"; }}/pkgs/development/guix/guix.nix" {  }; })]
]
