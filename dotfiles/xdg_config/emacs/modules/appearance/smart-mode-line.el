(use-package smart-mode-line
  :init
    (setq sml/theme 'dark)
    (setq sml/no-confirm-load-theme t)
    (setq column-number-mode t)

  :config
    ;; bootstrap sml
    (sml/setup)

    ;; force only filename to display
    (setq sml/directory-truncation-string "")
    (setq sml/name-width '1)

    ;; don't show mule-info
    (setq sml/mule-info "")

    ;; modified / external modified / read-only
    (setq sml/modified-char "* ")
    (setq sml/read-only-char "R ")
    (setq sml/outside-modified-char "M ")

    ;; extend bar all the way to border
    (setq sml/mode-width 'right)

    ;; don't show prefixes
    (setq sml/prefix-regexp nil)
    (setq sml/replacer-regexp-list nil)

    ;; don't prepend unnecessary spaces to row or col
    (setq sml/line-number-format " %l")
    (setq sml/col-number-format "%c")

    ;; don't show - or @ before filename for local/remote
    (setq sml/show-remote nil)

    ;; don't show vc info
    (add-hook 'buffer-list-update-hook (lambda () (setq vc-mode "")))

    ;; separators
    (setq sml/pre-id-separator "")
    (setq sml/pos-id-separator "  ")
    (setq sml/pre-modes-separator "  ")

    ;; colors
    (defvar my/modeline-foreground "green")
    ;;;; modeline
    (set-face-background 'mode-line "color-236")
    (set-face-background 'mode-line-inactive "color-234")
    ;;;; line-number, numbers-separator, col-number
    (set-face-attribute 'sml/line-number nil :weight 'normal)
    (set-face-foreground 'sml/line-number "gray60")
    (set-face-foreground 'sml/col-number "gray60")
    ;;;; read-only, outside-modified, modified
    (set-face-attribute 'sml/modified nil :weight 'normal)
    ;;;; filename
    (set-face-foreground 'sml/filename my/modeline-foreground)
    (set-face-attribute 'sml/filename nil :weight 'normal)
    ;;;; position percentage
    (set-face-foreground 'sml/position-percentage "gray60")
    ;;;; modes
    (set-face-foreground 'sml/modes my/modeline-foreground)
    ;;;; minor-modes
    (set-face-foreground 'sml/minor-modes "gray60")
    ;;;; other
    (set-face-foreground 'sml/client my/modeline-foreground)
    (set-face-foreground 'sml/prefix my/modeline-foreground)
    (set-face-foreground 'font-lock-warning-face my/modeline-foreground)
)
