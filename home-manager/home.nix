{ config, pkgs, lib, inputs, ... }:
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
  home.stateVersion = "25.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  ############

  # We'll the necessary system-wide options in the vsr namespace.
  # All the options we use are declared in options.nix.
  # For now, it's just one.
  vsr.isWorkMachine = builtins.getEnv "HOSTNAME" == "bold-bean";

  # Sets up agenix for secret management
  age = {
    identityPaths = [ "${config.home.homeDirectory}/keychain/vsr-secrets/id_ed25519" ];
    secrets = {
      # This is only half a secret; I just don't want to leak IPs to the public on GitHub, but I'm
      # the only user on my machine, so I don't really care if these get decrypted to an easily accessible file.
      sshWorkServersData = {
        file = ./secrets/work-servers-data.age;
        path = "${config.home.homeDirectory}/.ssh/work-servers-data";
      };
    };
  };

  home.packages = [
    inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];


  imports = [
    ./options.nix
    ./fonts/config.nix
    ./programs/wofi.nix
    ./programs/utilities.nix
    ./programs/pass-and-gpg.nix
    ./programs/bash/config.nix
    ./programs/emacs/config.nix
    ./programs/vim/config.nix
    ./programs/ssh/config.nix
    ./programs/unison/config.nix
    ./programs/git.nix
    ./programs/papis/config.nix
  ];
}
