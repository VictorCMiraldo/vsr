{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "victor";
  home.homeDirectory = "/home/victor";

  # My environment variables
  home.sessionVariables = {
    VSR_ROOT = builtins.toString ./..;

    EDITOR = "vim";

    LANG = "en_US.utf8";
    LOCALES_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";

    # This fixes the bug where firefox crashes when a open-file dialog
    # is open.
    GTK_USE_PORTAL = 1;
  };

  # Manages our XDG user dirs
  xdg.userDirs = {
    enable = true;
    desktop = "$HOME/tmp/Desktop";
    documents = "$HOME/doc";
    download = "$HOME/tmp";
    templates = "$HOME/.templates";
    music = "$HOME/data/Music";
    videos = "$HOME/data/Videos";
    pictures = "$HOME/data/Pictures";
    createDirectories = true;
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  ############

  imports = [
    ./xmonad/config.nix
    ./fonts/config.nix
    ./programs/utilities.nix
    ./programs/pass-and-gpg.nix
    ./programs/bash/config.nix
    ./programs/emacs/config.nix
    ./programs/vim/config.nix
    ./programs/ssh/config.nix
  ];

  ############

  programs.git = {
    enable = true;
    userName = "Victor Miraldo";
    userEmail = "victor.miraldo@fastmail.com";
  };
}
