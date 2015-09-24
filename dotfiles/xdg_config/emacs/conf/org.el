;; don't show leading stars in org-mode
(setq org-hide-leading-stars t)
(add-hook 'org-mode-hook (lambda ()
  (set-face-foreground 'org-hide "black")))

;; only colorize the stars of org-mode headers
(setq org-level-color-stars-only t)

;; colorize TODO keywords
(setq org-todo-keyword-faces
  '(("TODO" . (:foreground "red" :weight bold))
    ("DONE" . (:foreground "green" :weight bold))))
