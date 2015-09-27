;; evil-leader
(use-package evil-leader

  :ensure evil
  :ensure evil-commentary
  :ensure expand-region
  :ensure yasnippet

  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")

  (defun my/fully-unindent ()
    "Delete all whitespace preceding visual selection."
    ;; clobbers "m" mark
    (interactive)
    (if evil-visual-state-local-minor-mode nil
      (evil-visual-line))
    (evil-exit-visual-state)
    (evil-goto-mark 062) ; >
    (evil-last-non-blank)
    (evil-set-marker 109) ; m
    (evil-goto-mark 060) ; <
    (evil-beginning-of-line)
    (evil-visual-char)
    (evil-goto-mark 109) ; m
    (evil-ex-call-command "`<,`>" "delete-whitespace-rectangle" nil)
    )

  ;; global
  (evil-leader/set-key
    ;; evil-commentary
    "c" 'evil-commentary-line
    ;; expand-region
    "e" 'er/expand-region
    ;; personal
    "<" 'my/fully-unindent
    "va" 'align-regex
  )

  ;; org mode
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
