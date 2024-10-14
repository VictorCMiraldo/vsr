# Configures my entire X session but leaves GTK untouched,
# that is better configured through lxappearance manually.
{config, pkgs, ...}:
let
  my-theme = import ./theme.nix;
in {
  imports = [ 
    ./wofi.nix 
    ./wofi-pass.nix 
    ./waybar.nix 
    ./xkb.nix 
  ];

  # Sway, when installed by default, it creates a desktop file that
  # loads sway without really loading our `.profile`. This means
  # we need to "manually" install the following file into:
  #
  # 1. /usr/share/wayland-sessions for GDM3 to display
  # 2. TODO ??? See what lightDM wants later.
  #
  home.file.".config/sway/startup.sh" = {
    executable = true;
    text = ''
      #! /bin/sh
      # Load important variables, bringing nix binaries into path
      . ${config.home.homeDirectory}/.profile

      # xsetroot -cursor_name left_ptr

      # Run sway
      sway
    '';
  };

  home.file.".config/sway/my-sway.desktop".text = ''
    [Desktop Entry]
    Name=Sway on a Shell
    Comment=Runs sway from a startup script. I'll have it my Sway, please.
    Exec=${config.home.homeDirectory}/.config/sway/startup.sh
    Type=Application
  '';

  home.file.".config/sway/install.sh" = {
    executable = true;
    text = ''
      #! /bin/sh

      # Set the themes we'd like to use.
      gsettings set org.gnome.desktop.interface icon-theme 'Papirus-Dark'

      cp ${config.home.homeDirectory}/.config/sway/my-sway.desktop /usr/share/wayland-sessions/
      '';
  };

  home.file.".config/sway/config".source = ./sway-config;
  home.file.".config/sway/binds.sway".source = ./sway-keybinds;
}
