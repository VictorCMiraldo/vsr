{config, pkgs, ...}:
{
  home.packages = [
    papis
  ];

  home.file.".config/papis/config" = ./config
}
