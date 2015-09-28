;; evil-leader
(use-package evil-leader

  :ensure evil
  :ensure evil-commentary
  :ensure expand-region
  :ensure yasnippet
  :ensure flycheck

  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")

  (evil-leader/set-key
    ;; plugins
    "c" 'evil-commentary-line
    "e" 'er/expand-region
    "l" 'flycheck-list-errors
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
