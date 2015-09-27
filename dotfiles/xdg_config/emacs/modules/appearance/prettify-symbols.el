;; js
(add-hook 'js-mode-hook (lambda ()
  (push '("function" . ?ƒ) prettify-symbols-alist)
))

;; activate mode
(add-hook 'buffer-list-update-hook 'prettify-symbols-mode)
