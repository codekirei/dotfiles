(use-package yasnippet
  :config
  (setq yas-snippet-dirs (concat my/config-dir "modules/yasnippet/snippets"))
  (setq yas/indent-line nil)
  (yas-global-mode 1)
  (diminish 'yas-minor-mode)
  (set-face-background 'yas-field-highlight-face "black")
)
