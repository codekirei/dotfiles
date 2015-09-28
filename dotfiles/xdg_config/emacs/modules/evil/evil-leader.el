;; evil-leader
(use-package evil-leader

  :ensure evil
  :ensure evil-commentary
  :ensure expand-region
  :ensure flycheck
  :ensure helm-ag
  :ensure helm-dash
  :ensure helm-swoop
  :ensure yasnippet

  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")

  (evil-leader/set-key
    ;; plugins
    "c" 'evil-commentary-line
    "/" 'helm-dash
    "d" 'helm-dash-at-point
    "e" 'er/expand-region
    "l" 'flycheck-list-errors
    "sa" 'helm-ag-project-root
    "ss" 'helm-swoop
    "sm" 'helm-multi-swoop-all
    ;; personal
    "<" 'my/fully-unindent
    "va" 'align-regex
    "p" 'check-parens
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
