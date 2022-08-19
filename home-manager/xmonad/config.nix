# Configures my entire X session but leaves GTK untouched,
# that is better configured through lxappearance manually.
{config, pkgs, ...}:
let
  my-theme = import ./theme.nix;
in {
  imports = [ 
    ./polybar.nix 
    ./rofi.nix 
  ];

  programs.feh.enable = true;

  home.file.".xmonad/install.sh" = {
    executable = true;
    text = ''
      #! /bin/bash
      gsettings set org.mate.session.required-components windowmanager xmonad
      gsettings set org.mate.session required-components-list "['windowmanager']"
    '';
  };

  home.file.".xmonad/uninstall.sh" = {
    executable = true;
    text = ''
      #! /bin/bash
      gsettings set org.mate.session.required-components windowmanager marco
      gsettings reset org.mate.session required-components-list
      # old value was: ['windowmanager', 'panel', 'filemanager', 'dock']
    '';
  };

  # We'll get xmonad compiling through the home-manager, but installation
  # will be manual, through a script:
  home.file.".local/share/applications/xmonad.desktop".text = ''
    [Desktop Entry]
    Type=Application
    Name=XMonad
    Exec=/home/victor/.xmonad/xmonadrc
    NoDisplay=true
  '';

  home.file.".xmonad/xmonadrc" = {
    executable = true;
    text = ''
      #! /bin/sh
      . ${config.home.homeDirectory}/.profile
      feh --bg-scale ${builtins.toString ./wallpaper.jpg} &
      ${config.home.homeDirectory}/.xmonad/xmonad-x86_64-linux
    '';
  };

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./XMonad.hs;
    # Define a custom 'Colors.hs' module, that will reflect our theme,
    # giving us access to these colors within XMonad.hs.
    libFiles = {
      "Reflection.hs" = pkgs.writeText "Reflection.hs" ''
        module Reflection where

        background, foreground, accent, ok, warn :: String
        background = "${my-theme.colors.bg}"
        foreground = "${my-theme.colors.fg}"
        ok = "${my-theme.colors.notify-ok}"
        warn = "${my-theme.colors.notify-warn}"
        accent = "${my-theme.colors.focus}"

        polybarCmd :: String
        polybarCmd = "polybar"
        '';
    };
  };
}
