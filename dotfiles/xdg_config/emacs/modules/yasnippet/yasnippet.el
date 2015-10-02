(use-package yasnippet
  :config
  (setq yas-snippet-dirs (concat my/config-dir "modules/yasnippet/snippets"))
  (setq yas/indent-line 'fixed)
  (yas-global-mode 1)
  (set-face-background 'yas-field-highlight-face "black")
  (set-face-foreground 'yas-field-highlight-face "white")
)
