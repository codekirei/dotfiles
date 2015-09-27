;; indentation
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset nil)

;; hooks
(defun my/python-hooks ()
  "Set python-specific things for python-mode"
  (setq tab-width 4)
  (setq tab-stop-list (number-sequence 4 8 4)))
(add-hook 'python-mode-hook 'my/python-hooks)
