{ config, pkgs, lib, ... }:
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

    # Sway-needed things
    # MOZ_ENABLE_WAYLAND=1;
    # GTK_USE_PORTAL=0;
    # XDG_CURRENT_DESKTOP="sway";
    # XDG_SESSION_DESKTOP="sway";
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
  home.stateVersion = "23.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  ############

  # We'll the necessary system-wide options in the vsr namespace.
  # All the options we use are declared in options.nix.
  # For now, it's just one.
  vsr.isWorkMachine = builtins.getEnv "HOSTNAME" == "dev-lt-111"
                   || builtins.getEnv "HOSTNAME" == "dev-lt-60";
  

  imports = [
    ./options.nix
    ./fonts/config.nix
    ./sway/config.nix
    ./programs/utilities.nix
    ./programs/pass-and-gpg.nix
    ./programs/bash/config.nix
    ./programs/emacs-wayland/config.nix
    ./programs/vim/config.nix
    ./programs/ssh/config.nix
    ./programs/git.nix
    ./programs/papis/config.nix
  ];
}
