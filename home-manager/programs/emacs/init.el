;; Bootstraps straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Bring in use-package through straight, so we can preserve most of the config
(straight-use-package 'use-package)

;; We don't want a poluted mode line
(use-package diminish :straight t)

;; Provides a nice centered text for writing
(use-package olivetti
  :straight t
  :hook (markdown . olivetti-mode)
  :custom
    (olivetti-body-width 110))

(use-package markdown-mode
  :straight t)

;; Making it easier to discover Emacs key presses.
;; This is actually pretty cool.
(use-package which-key
  :straight t
  :defer 5
  :diminish
  :config (which-key-mode)
          (which-key-setup-side-window-bottom)
          (setq which-key-idle-delay 0.7))

;; Can't survive without magit or timemachine
(use-package git-timemachine
  :straight t
  :defer 5)
(use-package magit
  :straight t
  :commands
    magit
  :custom
    (setq magit-diff-refine-hunk 'all)
  :config
    (global-set-key (kbd "C-x g") 'magit-status))

(use-package undo-tree
  :straight t
  :config (global-undo-tree-mode))

;; Set up evil mode
(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (diminish 'undo-tree-mode)
  (evil-mode 1)
  (global-set-key (kbd "<backtab>") #'evil-shift-left-line)
  :custom
  ;; Undo with undo-tree
  (evil-undo-system 'undo-tree)

  ;; I don't want autoindentation when opening lines with o or O
  (evil-auto-indent nil)

  ;; And I want > and < to shift lines one column at a time
  (evil-shift-width 1))
(use-package evil-leader
  :straight t
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ;; ']' next
      "] e"    'next-error
      "] b"    'next-buffer
      "] w"    'other-window

    ;; '[' pref
      "[ e"    'previous-error
      "[ b"    'previous-buffer
      "[ w"    'prev-window

    ;; 'w' window
      "w [" 'prev-window
      "w ]" 'other-window
      "w f" 'delete-other-windows
      "w k" 'delete-window
      "w h" 'split-window-vertically
      "w v" 'split-window-horizontally

    ;; 'b' buffer
      "b b" 'switch-to-buffer
      "b [" 'previous-buffer
      "b ]" 'next-buffer
      "b k" 'kill-buffer

    ;; 'x' execute
      "x d" 'dired
      "x g" 'magit-status

    ;; 'm' merge
      "m u" 'smerge-keep-upper
      "m l" 'smerge-keep-lower
      "m n" 'smerge-next

    ;; 'c' code
      "c d" 'xref-find-definitions
      "c D" 'xref-find-references
      "c a" 'align-regexp
      "c j" 'fill-paragraph

    ;; 'p' project
      "p f" 'projectile-find-file
      "p /" 'projectile-grep
      "p p" 'projectile-switch-project)

  ;; Enable evil-leader everywhere
  (global-evil-leader-mode))
(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-want-unimpaired-p nil)
  (evil-collection-init))
(use-package evil-surround
  :straight t
  :config (global-evil-surround-mode 1))

;; Set-up helm
(use-package helm
  :straight t
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
  :straight t
  :diminish
  :commands
    company-manual-begin
  :init
    ;; Use C-<tab> to manually start company mode at point.
    (bind-key* "C-<tab>" #'company-manual-begin)
  ;; Bindings when the company list is active.
  :bind (:map company-active-map
              ("C-d"   . company-show-doc-buffer) ;; In new temp buffer
              ("<tab>" . company-complete-common-or-cycle))
  :custom
    ;; Only 2 letters required for completion to activate.
    (company-minimum-prefix-length 2)

    ;; Search other buffers for completion candidates
    (company-dabbrev-other-buffers t)
    (company-dabbrev-code-other-buffers t)

    ;; Show candidates according to importance, then case, then in-buffer frequency
    (company-transformers '(company-sort-by-backend-importance
                            company-sort-prefer-same-case-prefix
                            company-sort-by-occurrence))

    ;; Flushright any annotations for a compleition;
    ;; e.g., the description of what a snippet template word expands into.
    (company-tooltip-align-annotations t)

    ;; Allow (lengthy) numbers to be eligible for completion.
    (company-complete-number t)

    ;; Show 10 items in a tooltip; scrollbar otherwise or C-s ^_^
    (company-tooltip-limit 10)

    ;; Edge of the completion list cycles around.
    (company-selection-wrap-around t)

    ;; Do not downcase completions by default.
    (company-dabbrev-downcase nil)

    ;; Even if I write something with the ‘wrong’ case, provide the ‘correct’ casing.
    (company-dabbrev-ignore-case nil)

    ;; Immediately activate completion.
    (company-idle-delay 4)
  :config
    (global-company-mode))

;; Projectile
(use-package projectile
  :diminish
  :straight t
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
  :straight t
  :commands helm-projectile-on)

(use-package linum-relative
  :diminish
  :straight t
  :custom
  ;; use relative numbers everywhere
  (linum-relative-global-mode t)
  ;; Make linum show the actual line on the current line
  (linum-relative-current-symbol ""))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Direnv Integration ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package envrc
  :diminish
  :straight t
  :demand
  :config
  (envrc-global-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Language Server ;;
;;;;;;;;;;;;;;;;;;;;;

(defun my/flymake-goto-next-error ()
  (interactive)
  (flymake-goto-next-error 1 '(:error) t))

(defun my/flymake-goto-next-warning ()
  (interactive)
  (flymake-goto-next-error 1 '(:warning) t))

(defun my/flymake-goto-prev-error ()
  (interactive)
  (flymake-goto-prev-error 1 '(:error) t))

(defun my/flymake-goto-prev-warning ()
  (interactive)
  (flymake-goto-prev-error 1 '(:warning) t))

(use-package eglot
  :straight t
  :config
    (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
    (diminish 'eldoc-mode)
  :custom
    ;; don't ask for closing the server connection,
    (eglot-autoshutdown t)

    ;; wait 5s before sending changes. I often find the default of 0.5s to quick
    ;; and makes my emacs a bit slower.
    (eglot-send-changes-idle-time 5)
    (eldoc-echo-area-prefer-doc-buffer t)
    (eldoc-echo-area-use-multiline-p 1)
  :config
    (evil-leader/set-key
      ;; Redefine next-error to use flymake's
      "] e" 'my/flymake-goto-next-error
      "e ]" 'my/flymake-goto-next-error
      "e [" 'my/flymake-goto-prev-error
      "[ e" 'my/flymake-goto-prev-error
      "] W" 'my/flymake-goto-next-warning
      "W ]" 'my/flymake-goto-next-warning
      "W [" 'my/flymake-goto-prev-warning
      "[ W" 'my/flymake-goto-prev-warning))

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(use-package python-mode
  :straight t
  :mode ("\\.py\\'" . python-mode))

;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

(setq loc-stack-list '())
(defun loc-stack-push ()
  (interactive)
  (setq loc-stack-list ((buffer-name) . (mark)) . loc-stack-list))

(use-package haskell-mode
  :straight t
  :mode ("\\.hs\\'" . haskell-mode)
  ;; Do NOT turn the HLS immediately, for some projects it can be VERY slow.
  ;; :hook (haskell-mode . eglot-ensure)
  :bind (:map haskell-mode-map
    ("<f8>"        . haskell-navigate-imports)
    ("C-c M-e"     . haskell-goto-first-error)
    ("C-c C-l"     . haskell-process-load-file)
    ("C-c C-k"     . haskell-process-kill)
    ("C-c C-z"     . haskell-interactive-switch)
    ("C-c C-t"     . haskell-process-do-type)
    ("C-c C-i"     . haskell-process-do-info)
    ("C-c C-n C-c" . haskell-process-cabal-build)
    ("C-c C-n c"   . haskell-process-cabal))
  :custom-face
    (haskell-keyword-face ((t (:inherit font-lock-keyword-face))))
    (haskell-operator-face ((t (:inherit font-lock-keyword-face))))
  :custom
    ;; I don't want errors in a separate buffer
    (haskell-interactive-popup-errors nil)

    ;; sets up ormolu as our reformatter
    (haskell-mode-stylish-haskell-path "ormolu")

    ;; Keep my code indented with 2 spaces
    (haskell-indent-offset 2)

    ;; set the relevant options to pass around to cabal repl, ghci and stacj.
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
    (haskell-literate-default 'tex)

    ;; use some unicode symbols for us
    (haskell-font-lock-symbols t)
    (haskell-font-lock-symbols-alist
     '(("\\" . "λ")
       ("->" . "→")
       ("<-" . "←")
       ("=>" . "⇒")
       ("==" . "≡")
       ("/=" . "≢")
       (">=" . "≥")
       ("<=" . "≤")
       ("!!" . "‼")
       ("&&" . "∧")
       ("||" . "∨")
       ("~>" . "⇝")
       ("<~" . "⇜")
       ("><" . "⋈")
       ("-<" . "↢")
       ("::" . "∷")
       ("." "∘" haskell-font-lock-dot-is-not-composition)
       ("forall" . "∀")))
  :config
    (evil-leader/set-key
      ;; 'c' code
      "c f" 'haskell-mode-stylish-buffer
      "c K" 'haskell-process-kill
      "c s" 'eglot)
)

;;;;;;;;;;
;; Agda ;;
;;;;;;;;;;

(setq agda-mode-path
  (let ((coding-system-for-read 'utf-8))
        (shell-command-to-string "agda-mode locate")))

(when (file-exists-p agda-mode-path)
  (use-package agda-mode
    :no-require
    :mode ("\\.agda\\'" . agda-mode)
    :init
      (load-file agda-mode-path)
    :config
      (evil-leader/set-key
        ;; 'c' code
        "c l" 'agda2-load
        "c d" 'agda2-goto-definition
        "c b" 'agda2-go-back
        "c g" 'agda2-next-goal)
    :bind
      (:map agda2-mode-map
        ("M-<right>"   . agda2-goto-definition)
        ("M-<left>"    . agda2-go-back)
        ("M-<up>"      . agda2-previous-goal)
        ("M-<down>"    . agda2-next-goal)
       :map evil-normal-state-map
        ([mouse-2]     . agda2-goto-definition-mouse))
    :custom
       ;; use font-lock for agda2; maybe one day we sit and carefully customize things.
       (agda2-highlight-face-groups 'default-faces)
       (agda2-program-args nil)
       (agda2-program-name "agda"))

  ;; Font hack adapted from: https://stackoverflow.com/questions/33074370/how-can-i-use-a-different-ttf-fonts-for-certain-utf-8-characters-in-emacs
  ;; Add some specific font points that need to be displayed with
  ;; another font; and which font to use for them. Ranges and single points
  ;; are supported:
  ;;
  ;; (#x12345 . "ReplacementFont")
  ;; ((#x12300 . #x12333) "ReplacementFont")
  ;;
  (setq vcm/lacking-font-points '(
    ((#x1d552 . #x1d56b) . "DejaVu Sans") ;; lowecase bb: 𝕥, 𝕗, ...
  ))

  (when (fboundp 'set-fontset-font)
    (defun vcm/fix-unicode (&optional frame)
      (mapcar
        '(lambda (s) (set-fontset-font "fontset-default" (car s) (cdr s) frame))
        vcm/lacking-font-points)
    )
    (vcm/fix-unicode)
    (add-hook 'after-make-frame-functions 'vcm/fix-unicode))
)


;;;;;;;;;;;
;; LaTeX ;;
;;;;;;;;;;;

(use-package reftex
  :straight t
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
  :straight t
  :mode (("\\.lhs\\'" . LaTeX-mode)
         ("\\.tex\\'" . LaTeX-mode))
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

;; A tab is two spaces
(setq tab-width 2)

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

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

(use-package dired-x
  :straight (:type built-in)
  :init
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
  :custom
    (dired-omit-files "^\\...+$"))

(use-package dired-sidebar
  :straight t
  :commands (dired-sidebar-toggle-sidebar)
  :init
    (evil-leader/set-key "d" 'dired-sidebar-toggle-sidebar)
    (add-hook 'dired-sidebar-mode-hook
              (lambda ()
                (unless (file-remote-p default-directory)
                  (auto-revert-mode))))
  :custom
    (dired-sidebar-subtree-line-prefix "__")
  :config
   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
   (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

(use-package all-the-icons-dired
  :straight (:host github :repo "jtbm37/all-the-icons-dired"))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))


;;;;;;;;;;;;
;; Ricing ;;
;;;;;;;;;;;;

;; Dim inactive windows
(use-package dimmer
  :diminish
  :straight (:host github :repo "gonewest818/dimmer.el")
  :hook (after-init . dimmer-mode)
  :custom
    (dimmer-fraction 0.2)
    (dimmer-adjustment-mode :foreground)
    (dimmer-use-colorspace :rgb)
    (dimmer-watch-frame-focus-events nil)
    (dimmer-buffer-exclusion-regexps
       '("^ \\*transient\\*$" "^ \\*which-key\\*$" "^ \\*Minibuf-[0-9]+\\*$" "^ \\*Echo.*\\*$" ".*\\*eldoc\\*.*"))
  :config
    (dimmer-configure-which-key)
    (dimmer-configure-magit)
    (dimmer-configure-helm)
    (dimmer-configure-posframe))

(use-package doom-themes
  :straight t
  :custom
    (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
    (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

(use-package powerline
  :straight t
)

(use-package airline-themes
  :straight t
  :custom
  (airline-helm-colors t)
  :config
  (load-theme 'airline-base16_nord t))

;;;;;;;;;;;;;;;;;;;
;; Final Details ;;
;;;;;;;;;;;;;;;;;;;

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

;; Set support for a custom.el file.
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))

;; Easy to move around windows
(defun prev-window ()
  (interactive)
  (other-window -1))

;; New location for backups.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Silently delete execess backup versions
(setq delete-old-versions t)

;; Only keep the last 1000 backups of a file.
(setq kept-old-versions 10)

;; Even version controlled files get to be backed up.
(setq vc-make-backup-files t)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Use version numbers for backup files.
(setq version-control t)

;; Make it very easy to see the line with the cursor.
(global-hl-line-mode t)

;; Delete trailing whitespaces on saving
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; line numbers everywhere
(global-linum-mode t)
(linum-relative-toggle)

