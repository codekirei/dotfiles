;; evil mode
(use-package evil

  :demand t

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; deps
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  :ensure avy
  :ensure company
  :ensure evil-commentary
  :ensure evil-surround
  :ensure god-mode
  :ensure s

  :config

  ;; turn on modes
  (evil-mode 1)
  (evil-commentary-mode)
  (global-evil-surround-mode 1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; functions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq-default evil-shift-width my/tab-offset)
  (setq-default evil-auto-indent nil)
  (setq evil-ex-visual-char-range t)

  ;; fix bugged interaction with company
  (evil-declare-not-repeat 'company-complete)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; modeline
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; location
  (setq evil-mode-line-format '(after . mode-line-modes))
  ;; tag
  (setq
    evil-emacs-state-tag " Evil -- Emacs "
    evil-insert-state-tag " Evil -- Insert "
    evil-mode-line-tag " Evil "
    evil-motion-state-tag " Evil -- Motion "
    evil-normal-state-tag " Evil "
    evil-operator-state-tag " Evil -- Operator "
    evil-replace-state-tag " Evil -- Replace "
    evil-visual-state-tag " Evil -- Visual "
  )
  ;; message
  (setq
    evil-emacs-state-message ""
    evil-insert-state-message ""
    evil-motion-state-message ""
    evil-normal-state-message ""
    evil-operator-state-message ""
    evil-replace-state-message ""
    evil-visual-block-message ""
    evil-visual-char-message ""
    evil-visual-line-message ""
    evil-visual-state-state-message ""
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; keybinds
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; normal
  (my/define-keys evil-normal-state-map [
    ["C-u" evil-scroll-page-up]
    ["C-j" avy-goto-line]
    ["C-k" avy-goto-line]
    ["C-f" avy-goto-char]
    ["C-@" avy-goto-char-2]
    ["C-n" my/new-blank-line]
    ["C-m" (lambda () (interactive) (my/new-blank-line 1))]
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
    ["C-o" my/indent-block]
    ["C-e" yas-expand]
    ["C-j" my/open-below]
    ["TAB" tab-to-tab-stop]
    ["RET" my/indent-newline]
    ["C-l" company-complete]
    ["C-n" company-select-next]
    ["C-p" company-select-previous]
  ])
  ;; visual
  (my/define-keys evil-visual-state-map [
    ["|" my/pipe-and-replace]
  ])
  ;; org
  (add-hook 'org-mode-hook (lambda ()
    (my/define-keys org-mode-map [
      ["TAB" org-cycle]
      ["DEL" org-table-blank-field]
      ["C-t" org-cycle]
    ])))
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
