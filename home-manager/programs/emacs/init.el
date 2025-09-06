(require 'use-package)
(package-initialize)

;; This file defines a core emacs that has sensible behavior.

;; Set support for a custom.el file. We'll keep this outside nix for
;; good measure, so it can always be edited by the different bits
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))

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

    ;; Auto-save in /tmp plz
    (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

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

    (global-prettify-symbols-mode 1)
)

;; Profiling init
(use-package esup
  :vc (:url "https://github.com/jschaf/esup")
  :config
    (setq esup-depth 0)
)

(use-package notch
  :load-path "notch/"
  :custom
    ;; So here we try to get some sanity around TAB, which required my own package. LOL
    ;; Notch will load its indentation settings from `indent.el'.
    (indent-tabs-mode nil) ;; Does not allow indent to ever insert tabs
    (standard-indent 2) ;; configure how large the identation is.
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
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-undo-system 'undo-tree)

  :bind
    (:map
      evil-normal-state-map
        ("TAB" . #'notch-for-tab-command)
        ("<backtab>" . #'notch-back)
     :map
      evil-insert-state-map
        ("TAB" . #'notch-for-tab-command)
        ("<backtab>" . #'notch-back)
    )

  :custom
    (evil-shift-width 1)
    (evil-want-C-i-jump nil) ; Please don't hijack my tab nor C-i

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

(use-package annalist
  ;; annalist is a dependency of evil-collection
  :vc (:url "https://github.com/noctuid/annalist.el"))
(use-package evil-collection
  :vc (:url "https://github.com/emacs-evil/evil-collection" :rev "0.0.10")
  :after (:all evil annalist)
  :config
  (setq evil-collection-mode-list '(magit))
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
  :vc (:url "https://github.com/cofi/evil-leader")
  :after (evil)
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
      "c ." 'eglot-code-actions
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
      "r y" 'consult-yank-from-kill-ring

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
        ("SPC" . #'corfu-insert-separator)
        ("<tab>" . #'corfu-next)
        ("<escape>" . #'corfu-quit))
  :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :custom
    ;; Only use `corfu' when calling `completion-at-point'
    (corfu-auto nil)

    ;; Behave sanely w.r.t to the separator
    (corfu-quit-at-boundary 'separator)
    (corfu-separator ?\s)
    (corfu-quit-no-match 'separator)
    (corfu-preview-current 'insert)

    ;; Please cycle!
    (corfu-cycle t)
)

;;;;;;;;;;;;;;;;;;;;
;; Eglot + Direnv ;;
;;;;;;;;;;;;;;;;;;;;

(use-package inheritenv
  ;; inheritenv is a dependency of envrc 
  :vc (:url "https://github.com/purcell/inheritenv"))
(use-package envrc
  :vc (:url "https://github.com/purcell/envrc")
  :hook (after-init . envrc-global-mode)
  :after (diminish inheritenv)
  :init
    (diminish 'envrc-mode)
  :config
    (evil-leader/set-key
      "d r" 'envrc-reload)
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

    ;; If you can't find a way to stop the server from sending inlay-hints,
    ;; this is how you stop eglot from listening to them.
    (eglot-ignored-server-capabilities '(:inlayHintProvider))

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

    (add-to-list 'eglot-server-programs '(
      (python-mode python-ts-mode)
         "basedpyright-langserver" "--stdio" 
    ))

    (setq-default
       eglot-workspace-configuration
       '(:basedpyright (
           :typeCheckingMode "recommended"
         )
         ;; basedpyright is special: it needs a secton named `basedpyright.analysis`
         ;; and eglot doesn't interpret the dot hierarchically but as part of the section
         ;; name: https://github.com/DetachHead/basedpyright/issues/894
         :basedpyright.analysis (
           :diagnosticSeverityOverrides (
             :reportUnusedCallResult "none"
             :reportAny "none"
             :reportUnknownVariableType "hint"
             :reportUnusedParameter "hint"
             :reportInvalidCast "warning"
           )
           :inlayHints (
             :callArgumentNames :json-false
             :callFunctionReturnTypes :json-false
             :genericTypes :json-false
             :variableTypes :json-false
           )
         )))

    ;; And if you want to give eglot some per-workspace configuration, this is
    ;; how it would look like:
    ;; (setq-default eglot-workspace-configuration
    ;;             '((:haskell . (:formattingProvider "fourmolu"))))

)

;;;;;;;;;
;; Git ;;
;;;;;;;;;

(use-package magit
  :commands
    magit
   :bind
     ;; Recover our tab to toggle sections. Rebinding TAB is an endless
     ;; aventure!
     (:map magit-mode-map
        ("<tab>" . #'magit-section-toggle)
     )
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

(use-package git-timemachine
  :vc (:url "https://codeberg.org/pidu/git-timemachine")
  :custom
    (git-timemachine-abbreviation-length 6)
  :config
    (evil-leader/set-key
      ;; 't' rebase
      "g t" #'git-timemachine-toggle
    )
)

(use-package git-link
  :vc (:url "https://github.com/sshaw/git-link")
  :config
    (evil-leader/set-key
      ;; 't' rebase
      "g l" #'git-link
    )
)

;;;;;;;;;;;;;;
;; Includes ;;
;;;;;;;;;;;;;;

(load (expand-file-name "~/.emacs.d/modules/ricing.el"))
(load (expand-file-name "~/.emacs.d/modules/treesit.el"))

;; Agda sometimes just loads automagically; I can't have that happening.
;; so I'll disable it here.
;; (load (expand-file-name "~/.emacs.d/modules/agda.el"))

;; Finally include the custom file; ignoring potentially non-existent
;; or bad files.
(ignore-errors (load custom-file))
