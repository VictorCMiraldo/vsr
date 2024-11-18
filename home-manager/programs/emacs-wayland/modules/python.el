;; python

(use-package pet
  :vc (:url "https://github.com/wyuenho/emacs-pet")
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package python
  :ensure nil ;; Don't install this, is builtin.
  :after (notch eglot) ;; require notch, so we can tweak the settings
  :custom
    ;; Notch settings for python:
    (notch-punctuation-is-eow t) ;; In python punctuation marks end of word.
    (standard-indent 4)
  :config
    ;; Please, don't pollute my buffer!
    (eglot-inlay-hints-mode -1)
)
