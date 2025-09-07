(setq agda-mode-path
  (let ((coding-system-for-read 'utf-8))
        (shell-command-to-string "agda-mode locate")))

(when (file-exists-p agda-mode-path)
  (load agda-mode-path)
  (message "Agda exists in: %s" agda-mode-path)

  ;; (defun my-agda2-mode-hook ()
  ;;   "Custom behaviours for `agda2-mode'."
  ;;   (message "Changing input method")
  ;;   (evil-insert-state)
  ;;   (set-input-method "Agda")
  ;;   (evil-normal-state))

  (use-package agda2-mode
    :ensure nil
    :init
      ; (add-hook 'agda2-mode-hook 'my-agda2-mode-hook)
    :mode ("\\.agda\\'" . agda2-mode)
    :config
      (evil-leader/set-key
        ;; navigation
        "] g" 'agda2-next-goal
        "[ g" 'agda2-previous-goal

        ;; hoping this doesn't mess eglot
        "c d" 'agda2-goto-definition
        "c l" 'agda2-load
        "c r" 'agda2-refine
        "c ." 'agda2-goal-and-context-and-inferred

        ;; 'a' agda
        "a c" 'agda2-make-case
        "a t" 'agda2-goal-type
      )
      (set-input-method "Agda")
    :bind
      (:map agda2-mode-map
        ("M-<right>"   . agda2-goto-definition)
        ("M-<left>"    . agda2-go-back)
        ("M-<up>"      . agda2-previous-goal)
        ("M-<down>"    . agda2-next-goal)
        ("C-c C-l"     . agda2-load)
        ("C-c C-r"     . agda2-refine)
        ("C-c C-,"     . agda2-goal-and-context)
        ("C-c C-."     . agda2-goal-and-context-and-inferred)
        ("C-c C-;"     . agda2-goal-and-context-checked)
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
