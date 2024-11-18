;; Themeing

(unless (package-installed-p 'themes)
  (package-vc-install "https://github.com/doomemacs/themes"))

(use-package doom-themes
  :ensure nil
  :custom
    (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
    (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
    (load-theme 'doom-nord t)
)

(unless (package-installed-p 'nerd-icons)
  (package-vc-install "https://github.com/rainstormstudio/nerd-icons.el"))
(use-package nerd-icons
  :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono")
)

(unless (package-installed-p 'nerd-icons-dired)
  (package-vc-install "https://github.com/rainstormstudio/nerd-icons-dired"))
(use-package nerd-icons-dired
  :hook
    (dired-mode . nerd-icons-dired-mode)
)

(unless (package-installed-p 'f)
  (package-vc-install "https://github.com/rejeep/f.el"))
(unless (package-installed-p 's)
  (package-vc-install "https://github.com/magnars/s.el"))
(unless (package-installed-p 'shrink-path)
  (package-vc-install "https://github.com/zbelial/shrink-path.el"))
(unless (package-installed-p 'doom-modeline)
  (package-vc-install "https://github.com/seagle0128/doom-modeline"))

(use-package doom-modeline
  :ensure t
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
