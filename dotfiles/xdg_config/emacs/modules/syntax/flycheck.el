(use-package flycheck
  :config
  ;; hide overlay
  (setq flycheck-highlighting-mode nil)
  (setq flycheck-indication-mode nil)

  ;; enable for particular modes
  (add-hook 'js-mode-hook 'flycheck-mode)
  (setq flycheck-eslintrc (concat my/config-dir "modules/syntax/eslintrc"))

  ;; function to kill flycheck buffer
  (defun my/kill-flycheck-buffer ()
    "Kill flycheck buffer"
    (interactive)
    (kill-buffer flycheck-error-list-buffer))

  ;; change prefix so file-watchers aren't thrown off
  (setq flycheck-temp-prefix ".flycheck-temp")

  ;; format
  (setq flycheck-error-list-format '[
    ("Line" 4 flycheck-error-list-entry-< :right-align t)
    ("Col" 4 nil :right-align t)
    ("Level" 8 flycheck-error-list-entry-level-<)
    ("ID" 20 t)
    ("Message" 0 t)
    ;; (" (Checker)" 8 t)
  ])

  ;; colors
  (set-face-foreground 'flycheck-error-list-error "red")
  (set-face-foreground 'flycheck-error-list-warning "brightblack")
  (set-face-foreground 'flycheck-error-list-line-number "brightblack")
  (set-face-foreground 'flycheck-error-list-column-number "brightblack")
  (set-face-foreground 'flycheck-error-list-id "brightblack")
)
