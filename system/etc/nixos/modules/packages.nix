inputs@{ config, pkgs, sources, lib, ... } : { environment.systemPackages = import ../packages.nix inputs; }
