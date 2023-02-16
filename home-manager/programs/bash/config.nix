{config, pkgs, ...}:
{
  # Bare minimum bash configuration; I want to continue using text files for my
  # bashrc and profile, but I do want nix-direnv integration, so I'll just steal
  # the relevant parts from the respective home-manager modules
  home.packages = [ 
    pkgs.direnv 
    pkgs.nix-direnv 
  ];

  # Setup direnv to rely on nix-direnv
  xdg.configFile."direnv/direnvrc".text = ''
    source ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
    '';

  home.file.".bashrc".text = builtins.readFile ./bashrc + ''
      GPG_TTY=$(tty) 
      export GPG_TTY
  '';

  home.file.".profile".source = ./profile;
  home.file.".bash_aliases".source = ./bash_aliases;
}
