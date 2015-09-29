(use-package flycheck
  :config
  ;; (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode)
  (diminish 'flycheck-mode)
  (setq flycheck-highlighting-mode nil)
  (setq flycheck-indication-mode nil)
  (setq flycheck-error-list-minimum-level "error")
)
