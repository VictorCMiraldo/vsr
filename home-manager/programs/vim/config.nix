{config, pkgs, ...}:
{
  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ 
      vim-smt2 
    ];
    extraConfig = builtins.readFile ./vimrc;
  };
}
