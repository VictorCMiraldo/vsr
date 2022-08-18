# Configures my entire X session but leaves GTK untouched,
# that is better configured through lxappearance manually.
{pkgs, ...}:
let
  my-theme = import ./theme.nix;
in {
  imports = [ 
    ./polybar.nix 
    ./rofi.nix 
  ];

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

  # We'll bring in feh, needed to set our wallpaper
  home.file.".wallpaper.jpg".source = ./wallpaper.jpg;
  programs.feh = {
    enable = true;
  };

  # Tells HM to manage X and uses XMonad as our window manager
  xsession = { 
    enable = true;
    # Start the nm-applet by itself; curiously, using its home-manager counterpart
    # does not load properly.
    initExtra = ''
      if [ -f "~/.screenlayout/default.sh" ]; then
        ~/.screenlayout/default.sh
      fi
      feh --bg-scale ~/.wallpaper.jpg &
      nm-applet &
      pasystray &
      mate-screensaver &
      '';
    windowManager.xmonad = {
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
  };
}
