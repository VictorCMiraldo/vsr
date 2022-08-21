{pkgs, ...}:
{
  home.packages = [ 
    # Brings in rofi and the plugins we rely on
    (pkgs.rofi.override { plugins = [ pkgs.rofi-calc ]; }) 
    pkgs.rofi-power-menu 
  ];

  # Load our rofi configuration
  home.file.".config/rofi/config.rasi".text = import ./rofi-config.nix;
}



