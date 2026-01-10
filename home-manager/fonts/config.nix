{ pkgs, ... }:
{
  fonts.fontconfig.enable = true;
  home.packages = [
     pkgs.nerd-fonts.hack
     pkgs.nerd-fonts.fira-code
     pkgs.nerd-fonts.symbols-only
   ];
}
