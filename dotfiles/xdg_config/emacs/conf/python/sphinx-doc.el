;; probs worth: flycheck, emacs-company jedi, pytest-el, tox.el
;; maybe worth: python3-info
;; venv: pyenv-mode, virtualenvwrapper, pyvenv
;; big: anaconda-mode, elpy, pungi
;;
;; sphinx-doc
(use-package sphinx-doc

  :config
  (add-hook 'python-mode-hook (lambda () (sphinx-doc-mode t)))
)
