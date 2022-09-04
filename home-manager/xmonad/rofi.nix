{pkgs, ...}:
{
  # We'll only use rofi-power-menu from nix, rofi will be installed through debian to avoid
  # weird GTK errors
  home.packages = [ 
    pkgs.rofi-power-menu 
  ];

  # Load our rofi configuration
  home.file.".config/rofi/config.rasi".text = import ./rofi-config.nix;
}



