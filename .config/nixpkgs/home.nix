{ pkgs, ... }:

{
  # ...other config, other config...

  programs.direnv.enable = true;
  programs.direnv.enableNixDirenvIntegration = true;
}
