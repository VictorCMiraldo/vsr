{ config, pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    initContent = ''
      # Dynamically set GPG_TTY for the current terminal session
      GPG_TTY=$(tty)
      export GPG_TTY

      # Inside the tab menu, ESC drops the menu instead of selecting an item
      bindkey -M menuselect '^[' send-break
    '';

    shellAliases = {
      ec = "emacsclient -t -a vim";
    };

    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
        "pass"
        "ssh"
        "systemd"
      ];
      theme = "strug";
    };

  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = true;
  };
}
