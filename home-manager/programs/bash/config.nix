{config, pkgs, ...}:
{
  # Bare minimum bash configuration; I want to continue using text files for my
  # bashrc and profile, but I do want nix-direnv integration, so I'll just steal
  # the relevant parts from the respective home-manager modules
  home.packages = [ pkgs.direnv pkgs.nix-direnv ];

  # We'll bringg in gpg and gpg-agent here because we're manually installing bash 
  # integration below. Moreover, this way, we have the same version of both tools.
  # If pass is complaining, its probably because gpg-agent was also installed through
  # apt. Remove it from apt and run `gpgconf --kill all` to force the newer one to run.
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
  };

  # Setup direnv to rely on nix-direnv
  xdg.configFile."direnv/direnvrc".text = ''
    source ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
    '';

  home.file.".bashrc".text = builtins.readFile ./bashrc + ''
      # GPG Integration and usage as SSH agent
      export GPG_TTY=$(tty)
      ${config.programs.gpg.package}/bin/gpg-connect-agent updatestartuptty /bye > /dev/null
      '';

  home.file.".profile".source = ./profile;
}
