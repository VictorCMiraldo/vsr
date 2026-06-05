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
    ./fonts/config.nix
    ./programs/git.nix
    ./programs/ssh/config.nix
    ./programs/bash/config.nix
    ./programs/emacs/config.nix
    ./programs/vim/config.nix
    ./programs/wofi.nix
    ./programs/pass-and-gpg.nix
    ./programs/unison/config.nix
    # ./programs/papis/config.nix
  ];
}
