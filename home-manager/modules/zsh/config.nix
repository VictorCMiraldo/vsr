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
    '';

    shellAliases = {
      ec = "emacsclient -t -a vim";
    };
  };

  # 2. Enable direnv with native nix-direnv caching
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = true; # Automatically hooks into Zsh!
  };
}
