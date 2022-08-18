{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "victor";
  home.homeDirectory = "/home/victor";

  # My environment variables
  home.sessionVariables = {
    EDITOR = "vim";
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

  home.packages = with pkgs; [
    unison
    arandr
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  ############

  imports = [
    ./xsession/config.nix
    ./fonts/config.nix
    ./programs/pass.nix
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
