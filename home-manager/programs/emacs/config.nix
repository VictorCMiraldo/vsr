{pkgs, ...}:
{
  programs.emacs.enable = true;
  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/early-init.el".source = ./early-init.el;
  home.file.".emacs.d/custom.el".source = ./custom.el;
}
