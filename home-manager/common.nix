{
  config,
  pkgs,
  lib,
  agenix,
  ...
}:
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # All hosts need agenix and a selection of fonts
  fonts.fontconfig.enable = true;
  home.packages = [
    agenix.packages.${pkgs.stdenv.hostPlatform.system}.default

    pkgs.nerd-fonts.hack
    pkgs.nerd-fonts.fira-code
    pkgs.nerd-fonts.symbols-only
  ];

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "victor";
  home.homeDirectory = "/home/victor";

  # Common environment used everywhere
  home.sessionVariables = {
    VSR_ROOT = builtins.toString ./..;

    EDITOR = "vim";

    LANG = "en_US.utf8";
    LOCALES_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };

  imports = [
    ./modules/xdg.nix
    ./modules/git.nix
    ./modules/ssh/config.nix
    ./modules/bash/config.nix
    ./modules/emacs/config.nix
    ./modules/vim/config.nix
    ./modules/wofi/config.nix
    ./modules/pass-and-gpg.nix
    ./modules/unison/config.nix
    # ./modules/papis/config.nix
  ];
}
