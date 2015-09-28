(use-package helm-dash
  :config
  (setq helm-dash-docsets-path (concat my/data-dir "/docsets"))

  ;; show results after 1 char
  (setq helm-dash-min-length 1)

  ;; browser to open to read docset
  (setq helm-dash-browser-func (lambda (url) (interactive)
    (shell-command (concat
      "firefox-developer --new-window "
      (shell-quote-argument url)))
  ))

  ;; mode hooks to activate docsets
  (add-hook 'js-mode-hook (lambda () (interactive)
    (setq-local helm-dash-docsets '("NodeJS" "JavaScript"))))
)
