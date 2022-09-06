{config, pkgs, ...}:
{
  home.packages = [
    pkgs.papis
  ];

  home.file.".config/papis/config".source = ./papis-config;
}
