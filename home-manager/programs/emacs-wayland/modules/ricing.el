;; Themeing

(use-package doom-themes
  :vc (:url "https://github.com/doomemacs/themes" :rev :newest)
  :config
    ;; Global settings (defaults)
      (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
            doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (load-theme 'doom-nord t)
)

(use-package nerd-icons
  :vc (:url "https://github.com/rainstormstudio/nerd-icons.el")
  :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono")
)


(use-package nerd-icons-corfu
  :vc (:url "https://github.com/LuigiPiucco/nerd-icons-corfu")
  :after (corfu)
)

(use-package nerd-icons-dired
  :vc (:url "https://github.com/rainstormstudio/nerd-icons-dired")
  :after (dired)
  :hook
    (dired-mode . nerd-icons-dired-mode)
)

(use-package f
  :vc (:url "https://github.com/rejeep/f.el"))
(use-package s
  :vc (:url "https://github.com/magnars/s.el"))
(use-package shrink-path
  :vc (:url "https://github.com/zbelial/shrink-path.el"))
(use-package doom-modeline
  :vc (:url "https://github.com/seagle0128/doom-modeline")
  :after (shrink-path f s)
  :custom
    (doom-modeline-buffer-file-name-style 'buffer-name)
  :config
    ;; I'll define my own, custom doom-modeline, thanks!
    (doom-modeline-def-modeline 'my-line
      '(modals vcs check buffer-info buffer-position selection-info)
      '(misc-info minor-modes input-method buffer-encoding process))

    (add-hook 'doom-modeline-mode-hook
              (lambda () (doom-modeline-set-modeline 'my-line 'default)))

    ;; Configure other mode-lines based on major modes
    (add-to-list 'doom-modeline-mode-alist '(haskell-mode . my-line))
    (add-to-list 'doom-modeline-mode-alist '(python-mode . my-line))

    (doom-modeline-mode 1)
)
