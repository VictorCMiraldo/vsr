(require 'use-package)
(package-initialize)

;; Set support for a custom.el file.
(setq custom-file (expand-file-name "custom.el"))

;; I need a few packages that are not in ELPA, and
;; I don't feel like adding the entirety of MELPA
;; just for that. The package-vc-install can handle this
;; just fine! :D

(unless (package-installed-p 'haskell-ts-mode)
  (package-vc-install "https://codeberg.org/pranshu/haskell-ts-mode"))
(unless (package-installed-p 'evil-leader)
  (package-vc-install "https://github.com/cofi/evil-leader"))
(unless (package-installed-p 'annalist) ;; annalist is a dependency of evil-collection
  (package-vc-install "https://github.com/noctuid/annalist.el"))
(unless (package-installed-p 'evil-collection)
  (package-vc-install "https://github.com/emacs-evil/evil-collection"))
(unless (package-installed-p 'inheritenv) ;; inheritenv is a dependency of envrc
  (package-vc-install "https://github.com/purcell/inheritenv"))
(unless (package-installed-p 'envrc)
  (package-vc-install "https://github.com/purcell/envrc"))
(unless (package-installed-p 'emacs-reformatter)
  (package-vc-install "https://github.com/purcell/emacs-reformatter"))
(unless (package-installed-p 'themes)
  (package-vc-install "https://github.com/doomemacs/themes"))
(unless (package-installed-p 'doom-nano-modeline)
  (package-vc-install "https://github.com/ronisbr/doom-nano-modeline"))
(unless (package-installed-p 'nerd-icons)
  (package-vc-install "https://github.com/rainstormstudio/nerd-icons.el"))
(unless (package-installed-p 'nerd-icons-dired)
  (package-vc-install "https://github.com/rainstormstudio/nerd-icons-dired"))
(unless (package-installed-p 'nerd-icons-corfu)
  (package-vc-install "https://github.com/LuigiPiucco/nerd-icons-corfu"))

(use-package doom-themes
  :custom
    (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
    (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
    (load-theme 'doom-nord t)
)

(use-package doom-nano-modeline
  :custom
    (doom-nano-modeline-position 'bottom)
  :config
    (doom-nano-modeline-mode 1)
)

(use-package nerd-icons
  :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono")
)

(use-package nerd-icons-dired
  :hook
    (dired-mode . nerd-icons-dired-mode)
)

(use-package emacs
  :init
    (setq use-package-always-ensure t)

    ;; Some systems don't do file notifications well; see
    ;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
    (setopt auto-revert-interval 5)
    (setopt auto-revert-check-vc-info t)
    (global-auto-revert-mode)

    ;; Move through windows with Ctrl-<arrow keys>
    (windmove-default-keybindings 'control)

    ;; Save history of minibuffer, enabling up-arrow
    ;; to fetch the previous command.
    (savehist-mode)

    ;; Stop blinking the cursor
    (blink-cursor-mode -1)

    ;; Smooth scrolling: might end up with broken lines at the
    ;; top or bottom.
    (pixel-scroll-precision-mode)

    ;; Make it very easy to see the line with the cursor.
    (global-hl-line-mode t)

    ;; Delete trailing whitespaces on saving
    (add-hook 'write-file-hooks 'delete-trailing-whitespace)

  :custom
    (initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
    (display-time-default-load-average nil) ; this information is useless for most
    ;; Automatically reread from disk if the underlying file changes
    (auto-revert-avoid-polling t)

    ;; Fix archaic defaults
    (sentence-end-double-space nil)

    ;; don't show the spash screen, nor toolbar
    (inhibit-startup-screen t)

    ;; Mode line information
    (line-number-mode t) ; Show current line in modeline
    (column-number-mode t) ; Show column as well

    ; Use the minibuffer whilst in the minibuffer
    (enable-recursive-minibuffers t)
    (completion-cycle-threshold 1) ; TAB cycles candidates
    (completions-detailed t) ; Show annotations

    ;; So here we try to get some sanity around TAB.
    (tab-always-indent 'complete)
    (tab-first-completion 'eol)
    (indent-tabs-mode nil) ;; Does not allow indent to ever insert tabs
    (tab-width 2)
    (standard-indent 2)
    (indent-line-function #'indent-relative-first-indent-point)

    ;; Only show commands that apply to the current mode
    (read-extended-command-predicate #'command-completion-default-include-p)

    ;; Make backspace properly erase as many spaces as a tab
    (backward-delete-char-untabify-method 'hungry)

    (x-underline-at-descent-line nil) ; Prettier underlines
    (switch-to-buffer-obey-display-actions t) ; Make switching buffers more consistent

    (indicate-buffer-boundaries 'left) ; Show buffer top and bottom in the margin

    ;; Fix archaic defaults
    (sentence-end-double-space nil)

    ;; Centralized location for backups.
    (backup-directory-alist '(("." . "~/.emacs.d/backups")))

    ;; Silently delete execess backup versions
    (delete-old-versions t)

    ;; Only keep the last 1000 backups of a file.
    (kept-old-versions 10)

    ;; Even version controlled files get to be backed up.
    (vc-make-backup-files t)

    ;; Follow symlinks
    (vc-follow-symlinks t)

    ;; Use version numbers for backup files.
    (version-control t)

  :bind
    (:map minibuffer-mode-map
          ; TAB acts more like how it does in the shell: completes
          ; the maximum prefix until there are two options.
          ("TAB" . 'minibuffer-complete)
    )

  :config
    ;; Inhibit electric indent unless we say otherwise and
    ;; disable the mode.
    (setq-default electric-indent-inhibit t)
    (electric-indent-mode -1)

    ;; Make it very easy to see the line with the cursor.
    (global-hl-line-mode t)

    (setq major-mode-remap-alist
        '((haskell-mode . haskell-ts-mode)
          (bash-mode . bash-ts-mode)
          (python-mode . python-ts-mode)))

    (global-prettify-symbols-mode 1)
)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
)

(use-package diminish :ensure t)

;; Show line numbers in a relative manner
(use-package display-line-numbers
  :custom
    (display-line-numbers-type 'relative)
  :config
    (global-display-line-numbers-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; project.el niceties ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun local/current-project-root ()
  (interactive)
  (let ( (p (project-current nil)) )
       (if p
         (project-root p)
         (error "Not inside a project"))))

;; Define our own project.el backend. If we see a ".project.el" file
;; somewhere, that will indicate a project, each line of that file will
;; be a pattern used to ignore files.
(defun local/project-find-root (dir)
  (let* ( (override (locate-dominating-file dir ".project.el"))
          (dotfile (concat override ".project.el")) )
    (when (and override (file-readable-p dotfile))
      (let ((igns nil) (line ""))
	    (with-temp-buffer
	      (insert-file-contents-literally dotfile)
	      (goto-char (point-min))
	      (while (not (eobp))
		(setq line
                      (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                (unless (or (string= line "") (string-prefix-p "#" line))
                  (setq igns (cons line igns)))
                (forward-line 1)
              ))

            (list 'project-find-root override (cons ".project.el" igns))))))

(cl-defmethod project-ignores ((project (head project-find-root)) _dir)
  (car (cdr (cdr project))))

(cl-defmethod project-root ((project (head project-find-root)))
  (car (cdr project)))

(use-package project
  :config
    (setq project-vc-merge-submodules nil)
    (add-to-list 'project-find-functions #'local/project-find-root)
)


;; Evil!

(use-package evil
  :ensure t
  :init
    (setq evil-want-keybinding nil)
    (setq evil-undo-system 'undo-tree)

  :custom
    (evil-shift-width 1)

  :config
    (evil-mode)
    ;; special-mode buffers have a keymap defined and are meant
    ;; for things that emacs generates by itself. Better stick to
    ;; emacs input there!
    (evil-set-initial-state 'special-mode 'emacs)
    (evil-set-initial-state 'dired-mode 'emacs)
    ;; We need to drop into grep-mode into /normal/ mode, otherwise 'i' won't work.
    (evil-set-initial-state 'grep-mode 'normal)

    (setq evil-normal-state-cursor '(box "dark sea green"))
    (setq evil-insert-state-cursor '(bar "light blue"))
    (setq evil-visual-state-cursor '(hollow "orange"))
    (setq evil-emacs-state-cursor '(hollow "magenta"))
)
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-want-unimpaired-p nil)
  (evil-collection-init))
(use-package undo-tree
  :ensure t
  :diminish
  :custom
    (undo-tree-auto-save-history nil)
    (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
    (global-undo-tree-mode))

(defun prev-window ()
  (interactive)
  (other-window -1))

(use-package evil-leader
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ;; ']' next
      "] e" 'next-error
      "] b" 'next-buffer
      "] w" 'other-window

    ;; '[' pref
      "[ e" 'previous-error
      "[ b" 'previous-buffer
      "[ w" 'prev-window

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
      "x b" 'consult-buffer
      "x f" 'find-file

    ;; 'm' merge
      "m u" 'smerge-keep-upper
      "m l" 'smerge-keep-lower
      "m n" 'smerge-next

    ;; 'c' code
      "c d" 'xref-find-definitions
      "c D" 'xref-find-references
      "c j" 'fill-paragraph
      "c s" 'eglot
      "c c" 'comment-or-uncomment-region

    ;; 'p' project
      "p b" 'consult-project-buffer
      "p d" 'project-dired
      "p f" 'project-find-file
      "p G" 'consult-git-grep
      "p /" 'consult-ripgrep
      "p R" 'project-query-replace-regexp
      "p s" 'project-eshell

    ;; 'r' register
      "r y" 'helm-show-kill-ring

    ;; 'd' dired
      "d e" 'dired-create-empty-file

    ;; go
      "g g" 'avy-goto-char-timer

    ;; help
      "h ." 'eldoc-doc-buffer
      "h v" 'describe-variable
      "h f" 'describe-function
      "h k" 'describe-key
    )

  ;; Enable evil-leader everywhere
  (global-evil-leader-mode))

;;;;;;;;;;;;;;;;
;; Behavioral ;;
;;;;;;;;;;;;;;;;

;; Live-update candidates, a little like helm. Also
;; organize candidates into a vertical fashion, giving
;; us a lot of horizontal space in the minibuffer...
;; Check wiki for fun options! https://github.com/minad/vertico/wiki
(use-package vertico
  :ensure t
  :after embark
  :bind
    (:map vertico-map
          ; Makes M-TAB acts like how it does in the shell: completes
          ; the maximum prefix until there are two options.
          ("M-TAB" . #'minibuffer-complete)
          ("?" . #'minibuffer-completion-help)
    )
  :custom
    (vertico-cycle t)
  :config
    (vertico-mode)
)

;; ... this free'd up horizontal space is used by Marginalia
;; to give all sorts of nice contextual information.
(use-package marginalia
  :ensure t
  :config
    (marginalia-mode)
)

;; Orderless: powerful completion style
;; TODO: look at other completion styles
(use-package orderless
  :ensure t
  :custom
    (completion-styles '(orderless))
)

(use-package avy
  :ensure t
  :after embark
  :demand t
  :config
    ;; Add the option to run embark when using avy
    (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

    (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-embark)
)

(use-package embark
  :ensure t
  :demand t
  :bind (("C-;" . embark-act))
)

(use-package embark-consult :ensure t)

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; recentf-mode
;; savehist-mode

;; cool things: consult-project-buffer
;; idea: <LEADER> s l -- consult line
;; idea: <LEADER> s L -- consult multi line
;; Don't forget: #lalal -- -C2#lele
(use-package consult
  :ensure t
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))


;; Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
  :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
)

;;;;;;;;;;;;;;;;;;;;
;; Eglot + Direnv ;;
;;;;;;;;;;;;;;;;;;;;

(use-package envrc
  :after diminish
  :demand
  :init
    (diminish 'envrc-mode)
  :config
    (evil-leader/set-key
      "d r" 'envrc-reload)
    (envrc-global-mode)
)

;; We need markdown to render documentation.
(use-package markdown-mode
  :ensure t)

(defun vcm/flymake-goto-next-error ()
  (interactive)
  (flymake-goto-next-error 1 '(:error) t))

(defun vcm/flymake-goto-next-warning ()
  (interactive)
  (flymake-goto-next-error 1 '(:warning) t))

(defun vcm/flymake-goto-prev-error ()
  (interactive)
  (flymake-goto-prev-error 1 '(:error) t))

(defun vcm/flymake-goto-prev-warning ()
  (interactive)
  (flymake-goto-prev-error 1 '(:warning) t))

(use-package eglot
  :custom
    ;; don't ask for closing the server connection,
    (eglot-autoshutdown t)

    (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files

    (eglot-send-changes-idle-time 3)

    ;; I love docs, but let me open that buffer please!
    (eldoc-echo-area-prefer-doc-buffer t)
    (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)

  :config
    (diminish 'edoc-mode)
    ;; Set echo-area to be at most 3 lines
    (setq max-mini-window-height 3)


    (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event

    ;; Redefine next-error to use flymake's
    (evil-leader/set-key
      "] e" 'vcm/flymake-goto-next-error
      "e ]" 'vcm/flymake-goto-next-error
      "e [" 'vcm/flymake-goto-prev-error
      "[ e" 'vcm/flymake-goto-prev-error
      "] W" 'vcm/flymake-goto-next-warning
      "W ]" 'vcm/flymake-goto-next-warning
      "W [" 'vcm/flymake-goto-prev-warning
      "[ W" 'vcm/flymake-goto-prev-warning)

    ;; Sometimes you need to tell Eglot where to find the language server
    (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))

    ;; And if you want to give eglot some per-workspace configuration, this is
    ;; how it would look like:
    ;; (setq-default eglot-workspace-configuration
    ;;             '((:haskell . (:formattingProvider "fourmolu"))))

 )

;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

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

(use-package haskell-ts-mode
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

;;;;;;;;;
;; Git ;;
;;;;;;;;;

(use-package magit
  :commands
    magit
  :config
      (evil-leader/set-key
        ;; 'r' rebase
        "r k" #'git-rebase-move-line-up
        "r j" #'git-rebase-move-line-down
        "r s" #'git-rebase-squash
        "r w" #'git-rebase-reword
      )
  :custom
    (setq magit-diff-refine-hunk 'all)
)

;;;;;;;;;;;;;;;; Custom

(ignore-errors (load custom-file))

;;;;;;;;;;;;;;;; Indentation Playground


;; What we'd want:
;;
;; 45| .... lala
;; 46| ..c
;;
;; tab_press = do
;;   let prev_indent
;;   let this_indent =
;;         let p-to-0 = get text until beginning of line
;;          in p-to-0 ~ '[ \t]*' -- only spaces until the col 0
;;             `and` column p == k -- same col as previous line
;;
;;   -- is the point at the previous line's indent?
;;   let at_previous p = this_indent = prev_indent
;;
;;   case (at_begining p, at_previous p) of
;;      (True, False) -> add until at_previous
;;      (_, True) -> add tab-width spaces
;;      -- now, we're not at the beginning
;;      (False, False) ->
;;   let p = (point)
;;   if col p == o

;;   if is_beginning (point)
;;   then insert tab-width spaces
;;   else
;;   case point:
;;     beginning_of_line ->
;;       case previous_line_indent:
;;         0 -> insert tad-width spaces.
;;         n -> insert n spaces.
;;       indent as much as previous line on first press.

(defun my/prev-line-indent ()
  "Returns the indentation level of the previous non-empty line"
  (interactive) ;; to be removed
  (save-excursion
    (beginning-of-line)
    (if (re-search-backward "^[^\n]" nil t)
      (let ((end (save-excursion (forward-line 1) (point))))
        (or (looking-at "[ \t]")
            (skip-chars-forward "^ \t" end))
        (skip-chars-forward " \t" end)
        (current-column)
      )
    )
  )
)

(defun my/point-in-line-state ()
  "Returns this line's and point state. Returns:

      'in-empty-line when the line is empty.

      'in-bol when the point is at the beginning of a non-empty line.

      'in-eol when the point is at the end of a non-empty line.

      'in-middle when the point is at the middle of a non-empty line AND
         the prefix up to the point contains non-whitespace characters.

      INT when the point is not in the beginning of the line,
         but the prefix up to the point is only whitespace characters. The
         int is the identation level of this line: the column of the first non-whitespace
         character in this line."
  (interactive)
  (save-excursion
    (cond
      ;; Check for beginning or end of line. Beginning first.
      ((string-match "^[[:blank:]]*\n$" (thing-at-point 'line t))
        'in-blank-line)
      ((eolp)
        'in-eol)

      ;; Ok, not bol nor eol!
      ;; Now, try to skip backwards until we're not seeing a tab or a space.
      ;; if we skip nothing, we're mid word!
      ((= (skip-chars-backward " \t") 0)
        'in-middle)

      ;; If the above check skipped all the way to the beginning,
      ;; we are in the blank-prefix.
      ((= (current-column) 0)
        (skip-chars-forward " \t"))

      ;; Else, we're in the middle of a non-empty line.
      (t
        'in-middle))
  )
)


