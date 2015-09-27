(use-package evil-escape
  :ensure evil
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "kj")
  (setq-default evil-escape-delay 0.1)
  (diminish 'evil-escape-mode)
)
