;; ov.el -- overlays
(use-package volatile-highlights

  :config
  (volatile-highlights-mode t)
  (set-face-attribute 'vhl/default-face nil :inherit 'mode-line-inactive)
  (diminish 'volatile-highlights-mode)
)
