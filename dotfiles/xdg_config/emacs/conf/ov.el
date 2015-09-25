;; ov.el -- overlays
(use-package ov

  :disabled t
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; FUNCS
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun my/ov-clear-on-edit (_ov _after _beg _end &optional _length)
    "Clear current overlay if edited."
    (let ((inhibit-modification-hooks t))
      (if _after (ov-clear (ov-beg _ov) (ov-end _ov)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ORG MODE
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TODO DRY these up
  (defun my/org-ov-todo ()
    (ov-set (ov-match "TODO")
      'display "†"
      'modification-hooks '(my/ov-clear-on-edit)))
  (defun my/org-ov-done ()
    (ov-set (ov-match "DONE")
      'display "✓"
      'modification-hooks '(my/ov-clear-on-edit)))
  (defun my/org-ovs ()
    (my/org-ov-todo)
    (my/org-ov-done))
  ;; apply ovs
  (add-hook 'org-mode-hook 'my/org-ovs)
 ;(add-hook 'post-command-hook 'my/org-ovs)))
)
