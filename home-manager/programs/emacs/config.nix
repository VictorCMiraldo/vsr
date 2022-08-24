{pkgs, ...}:
{
  # We won't rely on home-manager's emacs. Let's keep the system one.
  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/early-init.el".source = ./early-init.el;
  home.file.".emacs.d/custom.el".source = ./custom.el;
}
