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

  home.packages = [
    pkgs.qalculate-gtk
  ];

  programs.feh.enable = true;

  # Installing xmonad is easy: register it as the window manager of our mate session
  # and disable all other required components:
  # - panels & docks will interfere with polybar's tray
  # - filemanager will interfere with some other graphical settings such
  #   as the cursor and the background. We still can use caja as a file manager.
  home.file.".xmonad/install.sh" = {
    executable = true;
    text = 
      if builtins.getEnv "HOSTNAME" == "dev-lt-60"
      then ''
        #! /bin/bash
        gsettings set org.mate.session.required-components windowmanager xmonad
        gsettings set org.mate.session required-components-list "['windowmanager', 'panel']"
        gsettings set org.freedesktop.ibus.panel.emoji hotkey "[]"
      ''
      else ''
        #! /bin/bash
        gsettings set org.mate.session.required-components windowmanager xmonad
        gsettings set org.mate.session required-components-list "['windowmanager']"
        # I really don'y want ibus intercepting C-.
        gsettings set org.freedesktop.ibus.panel.emoji hotkey "[]"
      '';
  };

  home.file.".xmonad/uninstall.sh" = {
    executable = true;
    text = ''
      #! /bin/bash
      gsettings set org.mate.session.required-components windowmanager marco
      gsettings reset org.mate.session required-components-list
      gsettings reset org.freedesktop.ibus.panel.emoji hotkey
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
      # Load important variables, bringing nix binaries into path
      . ${config.home.homeDirectory}/.profile

      # Set the bg and default cursor; Forgetting to set the cursor name will
      # result in xmonad inheriting the X-shaped default cursor
      feh --bg-scale ${builtins.toString ./wallpaper.jpg} &
      xsetroot -cursor_name left_ptr

      # If we're at our work machine, we need a mate-panel to use as a dock, since polybar refuses
      # to pick the dock up properly. Probably due to some ubuntu magic.
      ${if builtins.getEnv "HOSTNAME" == "dev-lt-60" then "mate-panel &" else ""}

      # Run xmonad
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
