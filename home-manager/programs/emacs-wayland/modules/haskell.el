;; Haskell

(use-package reformatter
  :vc (:url "https://github.com/purcell/emacs-reformatter")
  :config

  (defvar vcm/haskell-formatter-path "ormolu")
  (defvar vcm/haskell-formatter-args nil)
  (defun vcm/set-haskell-formatter-vars ()
    ;; We use stylish-haskell in most of Channable, so if the buffer is there, please change my default of ormolu!
    (let ((n (buffer-file-name)))
      (when (string-prefix-p "/home/victor/channable" n)
        (setq vcm/haskell-formatter-path "stylish-haskell")
        (setq vcm/haskell-formatter-args nil)

        ;; Unless we're in imaginator, obviously! :)
        (when (string-prefix-p "/home/victor/channable/imaginator" n)
          (setq vcm/haskell-formatter-path "ormolu")
          (setq vcm/haskell-formatter-args nil))

        ;; Or megaphone! Will we use ormolu everywhere one day?!
        (when (string-prefix-p "/home/victor/channable/megaphone" n)
          (setq vcm/haskell-formatter-path "ormolu")
          (setq vcm/haskell-formatter-args nil))

        ;; Or macgyver! Forumolu there! LOL
        (when (string-prefix-p "/home/victor/channable/macgyver" n)
          (setq vcm/haskell-formatter-path "fourmolu")
          (setq vcm/haskell-formatter-args nil))

        ;; Or sharkmachine-interface Forumolu there ook! LOL
        (when (string-prefix-p "/home/victor/channable/sharkmachine-interface" n)
          (setq vcm/haskell-formatter-path "fourmolu")
          (setq vcm/haskell-formatter-args nil)))
    )
  )

  (reformatter-define haskell-format
     :program vcm/haskell-formatter-path
     :args vcm/haskell-formatter-args)

  (defun vcm/haskell-format-buffer ()
    (interactive)
    (vcm/set-haskell-formatter-vars)
    (haskell-format-buffer))
)

(use-package haskell-ts-mode
  :vc (:url "https://codeberg.org/pranshu/haskell-ts-mode")
  :after (reformatter eglot)
  :hook
    (haskell-ts-mode
     .
     (lambda ()
        (push '("<-" . "←") prettify-symbols-alist)
        (push '("=>" . "⇒") prettify-symbols-alist)
        (push '("==" . "≡") prettify-symbols-alist)
        (push '("/=" . "≢") prettify-symbols-alist)
        (push '(">=" . "≥") prettify-symbols-alist)
        (push '("<=" . "≤") prettify-symbols-alist)
        (push '("!!" . "‼") prettify-symbols-alist)
        (push '("&&" . "∧") prettify-symbols-alist)
        (push '("||" . "∨") prettify-symbols-alist)
        (push '("~>" . "⇝") prettify-symbols-alist)
        (push '("<~" . "⇜") prettify-symbols-alist)
        (push '("><" . "⋈") prettify-symbols-alist)
        (push '("-<" . "↢") prettify-symbols-alist)
        (push '("::" . "∷") prettify-symbols-alist)
        (push '("forall" . "∀") prettify-symbols-alist)))

  :custom
    ;; Abso-freaking-lutely not! Leave my TAB alone!
    (haskell-ts-use-indent nil)
  :config
    (with-eval-after-load 'eglot (haskell-ts-setup-eglot))
    (evil-leader/set-key
      ;; 'c' code
      "c f" 'vcm/haskell-format-buffer
    )
)
