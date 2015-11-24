;; evil-leader
(use-package evil-leader

  :ensure evil
  :ensure evil-commentary
  :ensure expand-region
  :ensure fiplr
  :ensure flycheck
  :ensure helm-ag
  :ensure helm-dash
  :ensure helm-swoop
  :ensure ranger
  :ensure yasnippet

  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")

  (evil-leader/set-key
    ;; plugins
    "/" 'helm-dash
    "b" 'ido-switch-buffer
    "c" 'evil-commentary-line
    "d" 'helm-dash-at-point
    "e" 'er/expand-region
    "f" (lambda () (interactive) (helm-swoop :$query "fixme"))
    "ll" 'flycheck-list-errors
    "lk" 'my/kill-flycheck-buffer
    "lp" 'check-parens
    "r" 'ranger
    "sa" 'helm-ag-project-root
    "sm" 'helm-multi-swoop-all
    "ss" 'helm-swoop
    "t" 'fiplr-find-file
    ;; personal
    "<" 'my/fully-unindent
    "va" 'align-regexp
    "w" 'toggle-truncate-lines
  )

  (evil-leader/set-key-for-mode 'org-mode
    "h" 'org-table-previous-field
    "H" 'org-table-move-column-left
    "J" 'org-table-move-row-down
    "l" 'org-table-next-field
    "K" 'org-table-move-row-up
    "L" 'org-table-move-column-right
    "o" (lambda () (interactive) (org-table-insert-row t) (evil-insert-state 1))
    "O" 'org-table-insert-row
    "ab" 'org-table-sort-lines
    "-" 'org-table-insert-hline
  )

)
