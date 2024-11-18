;; python

(use-package python
  :ensure nil ;; Don't install this, is builtin.
  :after (notch) ;; require notch, so we can tweak the settings
  :custom
  ;; Notch settings for python:
  (notch-punctuation-is-eow t) ;; In python punctuation marks end of word.
  (standard-indent 4)
)
