(use-package ranger

  :ensure s

  :config
  (setq ranger-show-dotfiles t)
  (setq ranger-show-literal nil)

  ;; section width ratios
  (setq ranger-width-parents 0.25)
  (setq ranger-max-parent-width 0.25)
  (setq ranger-width-preview 0.50)

  ;; hide irrelevant info
  (defun my/ranger-hooks ()
    "Hooks to run in ranger buffers."
    (interactive)
    (setq mode-line-format nil)
    (linum-mode -1)
  )
  (add-hook 'ranger-mode-hook 'my/ranger-hooks)
  (add-hook 'ranger-parent-dir-hook 'my/ranger-hooks)

  ;; header text
  (defun my/ranger-dir-header ()
    "Sets text to display in header of ranger.el sections."
    (interactive)
    (last (s-split "/" (dired-current-directory) t)))
  (setq ranger-header-func 'my/ranger-dir-header)
  (setq ranger-parent-header-func 'my/ranger-dir-header)
  (setq ranger-preview-header-func (lambda () (interactive)
    (last (s-split "/" (buffer-file-name) t))))

  ;; colors
  (set-face-background 'hl-line "green")
  (set-face-foreground 'hl-line "black")
)
