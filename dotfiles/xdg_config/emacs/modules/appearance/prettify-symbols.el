;; js
(add-hook 'js-mode-hook (lambda ()
  (push '("function" . ?ƒ) prettify-symbols-alist)
  (push '("this." . ?_) prettify-symbols-alist)
  (prettify-symbols-mode)
))

;; org
(add-hook 'org-mode-hook (lambda ()
  (push '("TODO" . ?†) prettify-symbols-alist)
  (push '("DONE" . ?✓) prettify-symbols-alist)
  (prettify-symbols-mode)
))
