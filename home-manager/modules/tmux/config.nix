{ pkgs, ... }:
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

    # Wrap the package to force the correct environment variables
    # for tmux-open
    package = pkgs.symlinkJoin {
      name = "tmux-wrapped";
      paths = [ pkgs.tmux ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/tmux \
          --set EDITOR "emacsclient -t" \
          --set VISUAL "emacsclient -t"
      '';
    };

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
      {
        plugin = tmuxPlugins.sidebar;
        extraConfig = ''
          set -g @sidebar-tree-command 'tree -C --gitignore -L2'
        '';
      }
      tmuxPlugins.open
    ];
    extraConfig = ''
      # split panes using h and v, just like my emacs
      bind h split-window -h -c '#{pane_current_path}'
      bind v split-window -v -c '#{pane_current_path}'
      bind c new-window -c '#{pane_current_path}'
      unbind '"'
      unbind %

      set-option -g renumber-windows on

      # word separators for automatic word selection
      set-window-option -g word-separators ' @"=()[]'  # default => ‘ -_@’.
    '';
  };
}
