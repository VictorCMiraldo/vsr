;; So I just learned about use-package, then I learned
;; about https://github.com/alhassy/emacs.d; this really meant
;; I had to revamp my configuration. :)
;;
;; Set support for a custom.el file.
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))

;; Make all commands of the "package" module present.
(require 'package)

;; Internet repositories for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

;; Actually get "package" to work.
(package-initialize)
;; (package-refresh-contents) ;; I actually don't want to refresh contents on every init

;; Set up "use-package" to be installed at startup if its not yet installed
;; and tell it to always install packages we require.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Making it easier to discover Emacs key presses.
;; This is actually pretty cool.
(use-package which-key
  :defer 5
  :diminish
  :config (which-key-mode)
          (which-key-setup-side-window-bottom)
          (setq which-key-idle-delay 0.7))

;; Can't survive without magit or timemachine
(use-package git-timemachine 
  :defer 5)
(use-package magit
  :commands magit
  :config (global-set-key (kbd "C-x g") 'magit-status))

(use-package undo-tree
  :config (global-undo-tree-mode))

;; Set up evil mode
(use-package evil
  :config 
  (evil-mode 1)
  ;; evil-mode binds C-. I want it for changing buffers
  (eval-after-load "evil-maps"
    (dolist (map '(evil-motion-state-map
                   evil-insert-state-map
                   evil-normal-state-map
                   evil-emacs-state-map))
      (define-key (eval map) (kbd "C-.") nil))) 
  (global-set-key (kbd "<backtab>") #'evil-shift-left-line)
  :bind (:map evil-motion-state-map 
          ("[" . evil-backward-paragraph)
          ("]" . evil-forward-paragraph))
  :custom
  ;; Undo with undo-tree
  (evil-undo-system 'undo-tree)

  ;; I don't want autoindentation when opening lines with o or O
  (evil-auto-indent nil)

  ;; And I want > and < to shift lines one column at a time
  (evil-shift-width 1))
(use-package evil-space
  :config (evil-space-mode))
(use-package evil-magit)
(use-package evil-surround
  :config (global-evil-surround-mode 1))
(use-package powerline)
(use-package powerline-evil
  :config (powerline-evil-center-color-theme))

;; Set-up helm
(use-package helm
 :diminish
 :init (helm-mode t)
 :bind (("M-x"     . helm-M-x)
        ("C-x C-f" . helm-find-files)
        ("C-x b"   . helm-mini)     ;; See buffers & recent files; more useful.
        ("C-x r b" . helm-filtered-bookmarks)
        ("C-x C-r" . helm-recentf)  ;; Search for recently edited files
        ("C-c i"   . helm-imenu)
        ("C-h a"   . helm-apropos)

        :map helm-map
        ;; We can list ‘actions’ on the currently selected item by C-z.
        ("C-z" . helm-select-action)
        ;; Let's keep tab-completetion anyhow.
        ("TAB"   . helm-execute-persistent-action)
        ("<tab>" . helm-execute-persistent-action))
 :custom
 ;; I don't want helm involved with code completion
 (helm-mode-handle-completion-in-region nil))

;; Enable fancy autocomplete from company
(use-package company
  :diminish
  :config
  (global-company-mode)
  (setq ;; Only 2 letters required for completion to activate.
   company-minimum-prefix-length 2

   ;; Search other buffers for compleition candidates
   company-dabbrev-other-buffers t
   company-dabbrev-code-other-buffers t

   ;; Show candidates according to importance, then case, then in-buffer frequency
   company-transformers '(company-sort-by-backend-importance
                          company-sort-prefer-same-case-prefix
                          company-sort-by-occurrence)

   ;; Flushright any annotations for a compleition;
   ;; e.g., the description of what a snippet template word expands into.
   company-tooltip-align-annotations t

   ;; Allow (lengthy) numbers to be eligible for completion.
   company-complete-number t

   ;; Show 10 items in a tooltip; scrollbar otherwise or C-s ^_^
   company-tooltip-limit 10

   ;; Edge of the completion list cycles around.
   company-selection-wrap-around t

   ;; Do not downcase completions by default.
   company-dabbrev-downcase nil

   ;; Even if I write something with the ‘wrong’ case,
   ;; provide the ‘correct’ casing.
   company-dabbrev-ignore-case nil

   ;; Immediately activate completion.
   company-idle-delay 4)

  ;; Use C-<tab> to manually start company mode at point. 
  (bind-key* "C-<tab>" #'company-manual-begin)

  ;; Bindings when the company list is active.
  :bind (:map company-active-map
              ("C-d"   . company-show-doc-buffer) ;; In new temp buffer
              ("<tab>" . company-complete-common-or-cycle)))

;; Projectile
(use-package projectile
  :commands projectile-project-root
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t)
  (helm-projectile-on) ;; enable helm-projectile
  (defun string-empty-p (str) (string= "" str)))

;; Helm-projectile is great for opening files quickly,
;; tell it to be loaded whenever helm-projectile-on is called.
(use-package helm-projectile
  :commands helm-projectile-on)


;; Fancy snippets and very good company support.
(use-package yasnippet
  :diminish yas-minor-mode
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas/global-mode)
         ("C-c y m" . yas/minor-mode)
         ("C-c y r" . yas-reload-all)
         ("C-c y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-global-mode 1))

;; direnv is nice to have. I came across needing it a DFINITY
;; and might just bring it into my personal machine. 
;; Thanks John W. for the many snippets! :)
(use-package direnv
  :init
  (defconst emacs-binary-path (directory-file-name
                               (file-name-directory
                                (executable-find "emacsclient"))))
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (add-hook 'post-command-hook #'direnv--maybe-update-environment)
              (direnv-update-environment default-directory)))

  (add-hook 'haskell-mode-hook
            (lambda ()
              (add-hook 'post-command-hook #'direnv--maybe-update-environment)
              (direnv-update-environment default-directory)))


  (defun patch-direnv-environment (&rest _args)
    (setenv "PATH" (concat emacs-binary-path ":" (getenv "PATH")))
    (setq exec-path (cons (file-name-as-directory emacs-binary-path)
                          exec-path)))

  (advice-add 'direnv-update-directory-environment
              :after #'patch-direnv-environment)

  (add-hook 'git-commit-mode-hook #'patch-direnv-environment)
  (add-hook 'magit-status-mode-hook #'patch-direnv-environment))

;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

(setq loc-stack-list '())
(defun loc-stack-push ()
  (interactive)
  (setq loc-stack-list ((buffer-name) . (mark)) . loc-stack-list))

;; TODO: Use lsp-mode too?
(use-package haskell-mode
  :bind (:map haskell-mode-map
    ("<f8>"        . haskell-navigate-imports)
    ("C-c M-e"     . haskell-goto-first-error)
    ("M-<down>"    . haskell-goto-next-error)
    ("M-<up>"      . haskell-goto-prev-error)
    ("M-<right>"   . haskell-mode-jump-to-def-or-tag)
    ("C-c C-l"     . haskell-process-load-file)
    ("C-c C-k"     . haskell-process-kill)
    ("C-c C-z"     . haskell-interactive-switch)
    ("C-c C-t"     . haskell-process-do-type)
    ("C-c C-i"     . haskell-process-do-info)
    ("C-c C-n C-c" . haskell-process-cabal-build)
    ("C-c C-n c"   . haskell-process-cabal))
  :custom
  ;; I don't want errors in a separate buffer
  (haskell-interactive-popup-errors nil)

  ;; Keep my code indented with 2 spaces
  (haskell-indent-offset 2)

  ;; set the relevant options to pass around to cabal repl, ghci and stacj.
  (haskell-process-args-cabal-repl (quote ("--ghc-option='-ferror-spans +RTS -M12G -RTS'")))
  (haskell-process-args-ghci (quote ("+RTS -M12G -RTS" "-fshow-loaded-modules")))
  (haskell-process-args-stack-ghci
   (quote
    ("--ghci-options=-fshow-loaded-modules -ferror-spans +RTS -M12G -RTS" "--allow-different-user")))

  ;; Load imported modules into the interactive session
  (haskell-process-auto-import-loaded-modules t)

  ;; keep a log of the underlying haskell-process
  (haskell-process-log t)

  ;; where to find cabal
  (haskell-process-path-cabal "cabal")

  ;; don't tell me to remove unused imports
  (haskell-process-suggest-remove-import-lines nil)

  ;; automatically decide whether to use cabal or stack depending on the project directory
  (haskell-process-type (quote auto))

  ;; don't process tags on save
  (haskell-tags-on-save nil)

  ;; default literate haskell style
  (haskell-literate-default 'tex))

(use-package company-ghci) 
(push 'company-ghci company-backends)

(use-package linum-relative
  :custom
  ;; use relative numbers everywhere
  (linum-relative-global-mode t)

  ;; Make linum show the actual line on the current line
  (linum-relative-current-symbol ""))

;; Setting up unicode-fonts
(use-package persistent-soft)
(use-package unicode-fonts
  :config (unicode-fonts-setup))

;;;;;;;;;;
;; Agda ;;
;;;;;;;;;;

;; first declare where it is, then, if it exists loads everything.
;; TODO: Ask John how to do this with use-package
;;
;; (use-package agda-mode
;;    :no-require
;;    :init (load-file ...)
;;    :load-path agda-mode-path)
;; 
;; macro-step --> place cursos on '(' M-x macro exp
;;
(setq agda-mode-path
  (let ((coding-system-for-read 'utf-8))
        (shell-command-to-string "agda-mode locate")))

(when (file-exists-p agda-mode-path)
  (load-file agda-mode-path)
  (eval-after-load 'agda2-mode '(progn
    (define-key agda2-mode-map (kbd "M-<right>")
      'agda2-goto-definition-keyboard)
    (define-key agda2-mode-map (kbd "M-<left>")
      'agda2-go-back)
    (define-key agda2-mode-map (kbd "M-<up>")
      'agda2-previous-goal)
    (define-key agda2-mode-map (kbd "M-<down>")
      'agda2-next-goal)
    (define-key evil-normal-state-map [mouse-2]
      'agda2-goto-definition-mouse)))

 (setq agda2-fontset-name "DejaVu Sans Mono 13")
 (setq agda2-program-args nil)
 (setq agda2-program-name "agda"))

;;;;;;;;;;
;; Rust ;;
;;;;;;;;;;

(defun my-rust-project-find-function (dir)
  (let ((root (locate-dominating-file dir "Cargo.toml")))
        (and root (cons 'transient root))))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  :custom
  ;; don't ask for closing the server connection,
  (eglot-autoshutdown t)

  ;; wait 1s before sending changes. I often find the default of 0.5s to quick
  ;; and makes my emacs a bit slower.
  (eglot-send-changes-idle-time 1))

;; The default Cargo Run window is read only, that's bad since I can't send
;; any input.
;;
;; https://emacs.stackexchange.com/questions/51156/cargo-process-does-not-accept-user-input
;;
(defun rust-compile-send-input ()
  "Read string from minibuffer and send it to the rust process of the current buffer."
  (interactive)
  (let ((input (read-from-minibuffer "Send input to rust process: "))
        (proc (get-buffer-process "*Cargo Run*"))
        (inhibit-read-only t))
    (process-send-string proc (concat input "\n"))))

;; Origami plays well with evil mode. In fact, evil
;; already has predefined commands:
;;  za - toggle fold
;;  zc - close
;;  zm - close all
;;  zr - open all
;;  zo - open
;;
(use-package origami
  :hook (rust-mode . origami-mode)
  :init
  ;; We need to tell origami how to work under rust mode
  (with-eval-after-load "origami"
    (add-to-list 'origami-parser-alist '(rust-mode . origami-c-style-parser)))
  :custom
  ;; Highlights the line the fold starts on
  (origami-show-fold-header t))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :bind (:map cargo-minor-mode-map ("C-c C-c C-y" . cargo-process-clippy))
  :bind (:map cargo-minor-mode-map ("C-c i"       . rust-compile-send-input))
  :config
  ;; Cargo mode sets "C-c C-c C-l" to cargo-process-clean. Its way to close to
  ;; C-C C-C C-k's process-check for my taste. I can do a clean on the terminal when needed.
  (global-unset-key (kbd "C-c C-c C-l"))
  (defadvice cargo-process-clippy
      (around my-cargo-process-clippy activate)
    (let ((cargo-process--command-flags (concat cargo-process--command-flags
                                                " --tests -- -D clippy::all")))
      ad-do-it)))

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . eglot-ensure)
  :config
  ;; Disable flymake's intrusive settings
  (setq flymake-no-changes-timeout 'nil)
  (setq flymake-start-check-on-newline 'nil)

  ;; Set up some hotkeys
  (add-hook 'rust-mode-hook
            #'(lambda ()
                (bind-key "M-<down>"   #'flymake-goto-next-error rust-mode-map)
                (bind-key "M-<up>"     #'flymake-goto-prev-error rust-mode-map)
                (bind-key "M-<right>"  #'xref-find-definitions rust-mode-map)
                (bind-key "C-c r"      #'xref-find-references rust-mode-map)
                (bind-key "C-c h"      #'eglot-help-at-point rust-mode-map)
                (bind-key "C-c C-c v" #'(lambda () (interactive) (shell-command "rustdocs std")) rust-mode-map)))


  ;; eglot uses project.el, and requires some special configuration to find
  ;; the root of the current project. Not ideal, but gets the job done.
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'my-rust-project-find-function))

  :custom
  (rust-format-on-save t))


;;;;;;;;;;;
;; LaTeX ;;
;;;;;;;;;;;

(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :custom
  ;; uses reftex to supply arguments to \ref, \cite, etc...
  (reftex-plug-into-AUCTeX t)

  ;; Give me a vertical TOC
  (reftex-toc-split-windows-horizontally t)

  ;; Makes TOC use only a small bit on the left, without deleting windows
  (reftex-toc-keep-other-windows t)

  ;; Include labels, chapter sections and subsections on the toc
  (reftex-toc-include-labels t)
  (reftex-toc-max-level 3)

  ;; follow include, input and lhsinclude commands. I often use lhsinclude
  ;; as a nifty hack: 
  ;; > \newcommand{\lhsinclude}[1]{}
  ;; > 
  ;; > ...
  ;; > 
  ;; > \chapter{Some Chapter}
  ;; > \label{chap:some-chapter}
  ;; > \lhsinclude{SomeChap.lhs} % lets reftex find references in src/SomeChap.lhs
  ;; > %include src/SomeChap.lhs % lets lhs2TeX include the file.
  (reftex-include-file-commands (quote ("include" "input" "lhsinclude")))

  ;; search for references in these files, must enable try-all-extensions too
  (reftex-file-extensions (quote (("tex" ".tex" ".ltx" ".lhs") ("bib" ".bib"))))
  (reftex-try-all-extensions t))

(use-package auctex
  :mode "\\.tex\\'"
  :mode "\\.lhs\\'"
  :config
  ;; I want to turn off spell-checking inside math
  ;; and code enviroments. Good documentation for this
  ;; in
  ;;  https://tex.stackexchange.com/questions/117204/skip-spelling-in-emacs-for-the-content-of-a-user-macro
  ;;  https://emacs.stackexchange.com/questions/5645/how-to-prevent-ispell-from-checking-inside-mathematical-formulae
  (add-hook 'LaTeX-mode-hook '(lambda ()
    (setq ispell-tex-skip-alists
       (list (append (car ispell-tex-skip-alists)
                     '( ("[^\\]\\$" . "[^\\]\\$")
                        ("\\\\begin{code}" . "\\\\end{code}$")
                      ))
             (cadr ispell-tex-skip-alists)))))
  :custom
  (TeX-auto-save t)
  (TeX-file-extensions
   (quote
    ("tex" "sty" "cls" "ltx" "texi" "txi" "texinfo" "dtx" "lhs")))
  (TeX-one-master "\\.\\(texi?\\|dtx\\|lhs\\)$"))

;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

;; Inhibit electric indent unless we say otherwise.
(setq-default electric-indent-inhibit t)
;; No electric indent anywhere (indents on pressing enter)
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; Make backspace properly erase as many spaces as a tab
(setq backward-delete-char-untabify-method 'hungry)

;; standard indent is two spaces for me
(setq standard-indent 2)

;; Lets tab complete
(setq tab-always-indent t)

;; Does not allow indent to ever insert tabs
(setq indent-tabs-mode nil)

;; Visualize tabs as "|" and show trailing whitespaces.
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode)


;;;;;;;;;;;;;;;;;;;
;; Global Config ;;
;;;;;;;;;;;;;;;;;;;

;; Load my modified zenburn version
(setq custom-safe-themes '("4528fb576178303ee89888e8126449341d463001cb38abe0015541eb798d8a23" "7a4efa993973000e5872099a3c24c310b8bb2568b70f3b9d53675e6edf1f3ce4" default))
(setq custom-theme-directory "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; * Emacs Parens
(show-paren-mode 1)

;; Easy to move around windows
(defun prev-window ()
  (interactive)
  (other-window -1))

;; * Some of my handy keys
(bind-key* "C-a" #'align-regexp)
(bind-key* "C-M-j" #'fill-paragraph)

(bind-key* "C-." #'other-window)
(bind-key* "C-," #'prev-window)

(bind-key* "C-<left>" #'previous-buffer)
(bind-key* "C-<right>" #'next-buffer)

;; Sometimes C-x 1 is too long. :)
(bind-key* "<f4>" #'delete-other-windows)

;; New location for backups.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Silently delete execess backup versions
(setq delete-old-versions t)

;; Only keep the last 1000 backups of a file.
(setq kept-old-versions 10)

;; Even version controlled files get to be backed up.
(setq vc-make-backup-files t)

;; Use version numbers for backup files.
(setq version-control t)

;; Make it very easy to see the line with the cursor.
(global-hl-line-mode t)

;; Delete trailing whitespaces on saving
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; No toolbar, please
(tool-bar-mode -1)

;; line numbers everywhere
(global-linum-mode t)
(linum-relative-toggle)

;; Show the column number on my powerline
(column-number-mode 1)

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Tunning lsp-mode according to
;; https://emacs-lsp.github.io/lsp-mode/page/performance/

;; Don't run GC all the time by enabling emacs to use ~100mb if memory
(setq gc-cons-threshold 100000000) 

;; Enable meacs to read more bytes from processes
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Some ispell configuration
;;
;; TODO: Give flyspell a fair shot
(setq ispell-dictionary "en_US")
(setq ispell-personal-dictionary "~/.emacs.d/aspell-dict")
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)

;; Allow shell commands to use at most 8 lines below the currently selected
;; buffer, whenever we launch a shell command from evil-ex
(setq display-buffer-alist
   (quote
    (("*Shell Command Output*"
      (display-buffer-reuse-window display-buffer-below-selected)
      (window-height . 8)))))

;; Note that ‘uniquify’ is builtin.
;; Gives us unique buffer names
(require 'uniquify)
(setq uniquify-separator "/"               ;; The separator in buffer names.
      uniquify-buffer-name-style 'forward) ;; names/in/this/style

;; Set up a larger font at my personal machine
(defun my-inc-fontsize ()
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         30)))

(when (string= system-name "Garlic")
  (progn (message "%s" "Setting larger font for Garlic")
         (my-inc-fontsize)))
