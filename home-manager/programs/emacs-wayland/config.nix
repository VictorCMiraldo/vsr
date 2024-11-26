{pkgs, ...}:
{
  home.packages = with pkgs; [
    emacs30-pgtk
  ];

  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/early-init.el".source = ./early-init.el;

  home.file.".emacs.d/modules/ricing.el".source = ./modules/ricing.el;
  home.file.".emacs.d/modules/agda.el".source = ./modules/agda.el;
  home.file.".emacs.d/modules/treesit.el".source = ./modules/treesit.el;

  # My own indentation package
  home.file.".emacs.d/notch/notch.el".source = ./notch/notch.el;

}
