{pkgs, ...}:
{
  home.packages = with pkgs; [
    emacs29-pgtk
  ];

  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/early-init.el".source = ./early-init.el;

  # My own indentation package
  home.file.".emacs.d/notch/notch.el".source = ./notch/notch.el;

}
