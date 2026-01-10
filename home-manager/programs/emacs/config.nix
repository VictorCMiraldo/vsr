{pkgs, ...}:
{
  home.packages = with pkgs; [
    lilypond
    emacs30-pgtk
  ];

  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/early-init.el".source = ./early-init.el;

  home.file.".emacs.d/modules/ricing.el".source = ./modules/ricing.el;
  home.file.".emacs.d/modules/agda.el".source = ./modules/agda.el;
  home.file.".emacs.d/modules/treesit.el".source = ./modules/treesit.el;

  home.file.".emacs.d/modules/lily.el".text = ''
    (use-package lilypond-mode
      :load-path "${pkgs.lilypond}/share/emacs/site-lisp"
      :mode (("\\.ly\\'" . LilyPond-mode)
             ("\\.ily\\'" . LilyPond-mode))
      :defer t
      :config
      ;; Set the path to the lilypond executable
      (setq LilyPond-command "${pkgs.lilypond}/bin/lilypond")

      ;; Optional: Set PDF viewer command (adjust to your preference)
      (setq LilyPond-pdf-command "xdg-open")

      ;; Optional: Set MIDI player command
      (setq LilyPond-midi-command "timidity")

      ;; Optional: Additional configuration
      (add-hook 'LilyPond-mode-hook
                (lambda ()
                  ;; Enable font-lock (syntax highlighting)
                  (turn-on-font-lock)
                  ;; Set compile command for C-c C-c
                  (setq-local compile-command
                              (concat "${pkgs.lilypond}/bin/lilypond " buffer-file-name)))))
  '';

  # My own indentation package
  home.file.".emacs.d/notch/notch.el".source = ./notch/notch.el;

}
