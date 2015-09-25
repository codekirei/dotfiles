(use-package base16-theme
  :init
    ;(load-theme 'base16-apathy-dark t))
    ;(load-theme 'base16-atelierheath-dark t))
    ;(load-theme 'base16-bespin-dark t))
    ;(load-theme 'base16-brewer-dark t))
    ;(load-theme 'base16-codeschool-dark t))
    ;(load-theme 'base16-embers-dark t))
    ;(load-theme 'base16-google-dark t))
    ;(load-theme 'base16-harmonic16-dark t))
    ;(load-theme 'base16-marrakesh-dark t))
    ;(load-theme 'base16-monokai-dark t))
    ;(load-theme 'base16-paraiso-dark t))
    ;(load-theme 'base16-pop-dark t))
    ;(load-theme 'base16-shapeshifter-dark t))
    (load-theme 'base16-solarized-dark t))
    ;(load-theme 'base16-summerfruit-dark t))

(defun my/bg-colorfix ()
  "Disable theme background color in terminal emacs."
  (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'my/bg-colorfix)
