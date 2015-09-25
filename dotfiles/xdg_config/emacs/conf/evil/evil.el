;; evil mode
(use-package evil

  :demand t

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; other packages
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  :ensure god-mode
  :ensure evil-commentary

  :config
  (evil-mode 1)
  (evil-commentary-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; functions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; match indentation of previous line on newline
  (evil-define-command my/indent-newline ()
    (interactive)
    (let ((saved-column (current-indentation)))
      (newline)
      (indent-to saved-column)))

  ;; TODO: figure out a nicer way to pass params to my/post-open-newline
  ;;       without spamming setq
  (defun my/open-newline (direction count)
    "Open newline above or below. Match current indentation."
    ;; custom vars
    (setq my/saved-column (current-indentation))
    (setq my/saved-line (line-number-at-pos))
    (setq my/saved-line-count (my/get-line-count))
    (setq my/saved-rep-count count)
    (setq my/open-newline-direction direction)
    ;; evil vars
    (setq evil-insert-count count)
    (setq evil-insert-lines t)
    ;; logic
    (add-hook 'evil-normal-state-entry-hook 'my/post-open-newline)
    (cond ((string= direction "below") (evil-insert-newline-below))
          ((string= direction "above") (evil-insert-newline-above)))
    (indent-to my/saved-column)
    (evil-insert-state 1))

  (defun my/open-above (count)
    (interactive "p")
    (my/open-newline "above" count))

  (defun my/open-below (count)
    (interactive "p")
    (my/open-newline "below" count))

  (defun my/post-open-newline ()
    (let* ((new-line-count (- (my/get-line-count) my/saved-line-count))
           (lines-per-group (/ new-line-count my/saved-rep-count))
           (lines-to-indent (* lines-per-group (- my/saved-rep-count 1))))
      (cond ((> my/saved-rep-count 1)
        (cond ((string= my/open-newline-direction "below")
               (evil-goto-line (+ my/saved-line lines-per-group)))
              ((string= my/open-newline-direction "above")
               (evil-goto-line (- (+ my/saved-line lines-per-group) 1))))
        (dotimes (i lines-to-indent)
          (evil-next-line)
          (indent-line-to (+ (current-indentation) my/saved-column))
        )
      ))
    )
    (remove-hook 'evil-normal-state-entry-hook 'my/post-open-newline)
  )

  ;; match paren, brace, bracket, quote
  ;; TODO: make these more intelligent
  (defun my/match-brace ()
    (interactive)
    (let ((saved-column (current-indentation)))
      (insert "{\n\n")
      (indent-to saved-column)
      (insert "}")
      (evil-previous-line)
      (indent-to (+ saved-column my/tab-offset))))
  (defun my/match-paren ()
    (interactive)
    (insert "()")
    (backward-char 1))
  (defun my/match-bracket ()
    (interactive)
    (insert "[]")
    (backward-char 1))
  (defun my/match-quote ()
    (interactive)
    (insert "\"\"")
    (backward-char 1))

  (defun my/evil-god-toggle ()
    "Toggle between evil and god mode"
    (interactive)
    (if (bound-and-true-p evil-mode)
      (progn
        (message "GOD MODE")
        (evil-mode 0)
        (god-local-mode))
      (progn
        (message "EVIL MODE")
        (evil-mode)
        (god-local-mode 0))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq-default evil-shift-width my/tab-offset)
  (setq-default evil-auto-indent nil)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; keybinds
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; normal
  (my/define-keys evil-normal-state-map [
    ["C-u" evil-scroll-page-up]
    ["j" evil-next-visual-line]
    ["k" evil-previous-visual-line]
    ["S" evil-substitute]
    ["s" evil-window-map]
    ["O" my/open-above]
    ["o" my/open-below]
    ["SPC" my/evil-god-toggle]
  ])
  ;; insert
  (my/define-keys evil-insert-state-map [
    ["C-j" my/open-below]
    ;["<backtab>" my/unindent]
    ["TAB" tab-to-tab-stop]
    ["RET" my/indent-newline]
    ["{" my/match-brace]
    ["\"" my/match-quote]
    ["(" my/match-paren]
    ["[" my/match-bracket]
  ])
  ;; visual
  (my/define-keys evil-visual-state-map [
    ["|" my/pipe-and-replace]
  ])
  ;; org
  (add-hook 'org-mode-hook (lambda ()
    ;; normal
    (my/define-keys evil-normal-state-map [
      ["TAB" org-cycle]
      ["t" org-cycle]
      ["DEL" org-table-blank-field]
    ])
    ;; insert
    (my/define-keys evil-insert-state-map [
      ["TAB" org-cycle]
    ])
    ))
  ;; god mode
  (add-hook 'god-local-mode-hook (lambda ()
    (my/define-keys god-local-mode-map [
      ["SPC" my/evil-god-toggle]
      ["." repeat]
    ])))
  ;; help
  (my/define-keys special-mode-map [
    ["s" evil-window-map]
  ])

)
