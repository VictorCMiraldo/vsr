{pkgs, ...}:
{
  home.packages = with pkgs; [
    emacs30-pgtk
  ];

  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/early-init.el".source = ./early-init.el;

  home.file.".emacs.d/modules/ricing.el".source = ./modules/ricing.el;
  home.file.".emacs.d/modules/haskell.el".source = ./modules/haskell.el;
  home.file.".emacs.d/modules/python.el".source = ./modules/python.el;

  # My own indentation package
  home.file.".emacs.d/notch/notch.el".source = ./notch/notch.el;

}
