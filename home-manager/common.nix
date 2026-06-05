{ config, pkgs, lib, agenix, ... }:
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
  };

  # Manages our XDG user dirs on things that are not a server
  xdg.userDirs = lib.mkIf (!config.vsr.isServer) {
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
  home.stateVersion = "25.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = [
    agenix.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];

  imports = [
    ./fonts/config.nix
    ./programs/git.nix
    ./programs/bash/config.nix
    ./programs/utilities.nix
    ./programs/emacs/config.nix
    ./programs/vim/config.nix
    ./programs/wofi.nix
    ./programs/pass-and-gpg.nix
    ./programs/ssh/config.nix
    ./programs/unison/config.nix
    ./programs/papis/config.nix
  ];
}
