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
    (setq-default eglot-workspace-configuration
      '(:basedpyright (
          ;; Type-checking is on "basic" for now becayse I can't get
          ;; basedpyright to obey the rest of the settings; so I'll
          ;; just turn it all off otherwise working on RM is impossible.
          :typeCheckingMode "basic"
          :analysis (
            :diagnosticSeverityOverrides (
              :reportUnusedCallResult :json-false
              :reportInvalidCast :json-false
            )
            :inlayHints (
              :callArgumentNames :json-false
              :functionReturnTypes :json-false
              :variableTypes :json-false
              :genericTypes :json-false
            )
          )
        )
      )
    )
)
