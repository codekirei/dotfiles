(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  ;; don't grab enter key
  (unbind-key (kbd "RET") company-active-map)

  ;; colors
  (defvar my/company-color "green")
  (set-face-background 'company-preview-common "black")
  (set-face-background 'company-scrollbar-bg "color-234")
  (set-face-background 'company-scrollbar-fg my/company-color)
  (set-face-background 'company-tooltip "color-236")
  (set-face-background 'company-tooltip-selection "brightblack")
  (set-face-foreground 'company-preview-common my/company-color)
  (set-face-foreground 'company-tooltip "white")
  (set-face-foreground 'company-tooltip-annotation my/company-color)
  (set-face-foreground 'company-tooltip-common my/company-color)
  (set-face-foreground 'company-tooltip-common-selection my/company-color)
)
