{ pkgs, ... }:
let
  tmux-menus = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "menus";
    version = "2.2.34";
    rtpPath = ".";
    src = pkgs.fetchFromGitHub {
      owner = "jaclu";
      repo = "tmux-menus";
      rev = "f60e791e80d066ef7463ab2a746f5f89a5683dc0";
      hash = "sha256-KPP6g1RX/c269IzhQeMO14a58sewqdTCaKgD3wfvDiw=";
    };
  };
in
{
  # tmux is cool and all... but paired with a file manager
  # is where it shines.
  programs.yazi = {
    enable = true;
  };

  programs.tmux = {
    enable = true;
    shell = "${pkgs.bash}/bin/bash";
    terminal = "tmux-256color";
    historyLimit = 10000;
    mouse = true;
    escapeTime = 0;
    baseIndex = 1;
    keyMode = "vi";
    prefix = "`";

    plugins = with pkgs; [
      tmuxPlugins.better-mouse-mode
      {
        plugin = tmuxPlugins.nord;
        extraConfig = ''
          # https://old.reddit.com/r/tmux/comments/mesrci/tmux_2_doesnt_seem_to_use_256_colors/
          set -g default-terminal "xterm-256color"
          set -ga terminal-overrides ",*256col*:Tc"
          set -ga terminal-overrides '*:Ss=\E[%p1%d q:Se=\E[ q'
          set-environment -g COLORTERM "truecolor"
        '';
      }
      tmuxPlugins.dotbar
      {
        plugin = tmux-menus;
        extraConfig = ''
          set -g @menus_use_cache 'No'
        '';
      }
    ];
    extraConfig = ''
      # split panes using h and v, just like my emacs
      bind v split-window -v -c '#{pane_current_path}'
      bind V split-window -h -c '#{pane_current_path}'
      bind c new-window -c '#{pane_current_path}'
      unbind '"'
      unbind %

      set-option -g renumber-windows on

      # word separators for automatic word selection
      set-window-option -g word-separators ' @"=()[]'  # default => ‘ -_@’.
    '';
  };
}
