{ pkgs, ... }:
{
  programs.tmux = {
    enable = true;
    shell = "${pkgs.bash}/bin/bash";
    terminal = "tmux-256color";
    histotryLimit = 10000;
    plugins = with pkgs; [
      tmuxPlugins.bette-rmouse-mode
    ];
    extraConfig = "";
  };
}
