;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Language Modes That Depend on tree-sitter ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I don't want to be installin tree-sitter grammars by hand; this setup
;; was taken and adapted from Combobulate.
(use-package treesit
  :ensure nil ;; built-in!
  :init
    (defun setup-install-grammars ()
      "Install Tree-sitter grammars if they are absent."
      (interactive)
      (dolist (grammar
               ;; Note the version numbers. These are the versions that
               ;; are known to work with Combobulate *and* Emacs.
               '((html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                 (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
                 (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                 (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                 (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
                 (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
                 (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                 (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                 (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1"))
                 (nix . ("https://github.com/nix-community/tree-sitter-nix"))
                )
              )
        (add-to-list 'treesit-language-source-alist grammar)
        (unless (treesit-language-available-p (car grammar))
          (message "Installing Grammar %s" (car grammar))
          (treesit-install-language-grammar (car grammar)))))

    (dolist (mapping
             '((python-mode . python-ts-mode)
               (js2-mode . js-ts-mode)
               (bash-mode . bash-ts-mode)
               (conf-toml-mode . toml-ts-mode)
               (json-mode . json-ts-mode)
               (js-json-mode . json-ts-mode)
               (haskell-mode . haskell-ts-mode)
               (nix-mode . nix-ts-mode)
             ))
      (add-to-list 'major-mode-remap-alist mapping))
  :config
    (setup-install-grammars)
)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(use-package pet
  :vc (:url "https://github.com/wyuenho/emacs-pet")
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package python
  :ensure nil ;; Don't install this, is builtin.
  :after (treesit notch eglot) ;; require notch, so we can tweak the settings
  :custom
    ;; Notch settings for python:
    (notch-punctuation-is-eow t) ;; In python punctuation marks end of word.
    (standard-indent 4)
)


;;;;;;;;;;;;;;;;
;; TypeScript ;;
;;;;;;;;;;;;;;;;


;; (use-package typescript
;;   :ensure nil
;;   :after (treesit)
;;   :mode (("\\.ts\\'" . typescript-ts-mode)
;;          ("\\.tsx\\'" . typescript-ts-mode))
;; )

(use-package tsx-mode
  :vc (:url "https://github.com/orzechowskid/tsx-mode.el" :branch "emacs30")
  :after (treesit notch eglot)
  :mode (("\\.ts\\'" . tsx-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
)

;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

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
  :after (treesit reformatter eglot)
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

;;;;;;;;;;
;; TLA+ ;;
;;;;;;;;;;

(use-package polymode
  :vc (:url "https://github.com/polymode/polymode"))

(use-package tla-pcal-mode
  :after (polymode)
  :vc (:url "https://github.com/mrc/tla-tools"))

;;;;;;;;;
;; Nix ;;
;;;;;;;;;

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :vc (:url "https://github.com/nix-community/nix-ts-mode")
)
