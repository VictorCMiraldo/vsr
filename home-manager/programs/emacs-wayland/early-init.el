;; No toolbar nor scrollbar, please
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Don't enable package
(setq package-enable-at-startup nil)

;; Tunning lsp-mode according to
;; https://emacs-lsp.github.io/lsp-mode/page/performance/

;; Don't run GC all the time by enabling emacs to use ~100mb if memory
(setq gc-cons-threshold 100000000)

;; Enable meacs to read more bytes from processes
(setq read-process-output-max (* 1024 1024)) ;; 1mb

