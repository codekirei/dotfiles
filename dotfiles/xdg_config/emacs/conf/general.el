;; turn off menu bar
(menu-bar-mode -1)

;; \n at end of file
(setq require-final-newline t)

;; whitespace
(global-whitespace-mode)
(setq whitespace-style '(face lines-tail tabs trailing empty))

;; parens
(show-paren-mode 1)
(setq show-paren-style 'expression)
(setq show-paren-delay 0)

;; litter (backups, autosaves, lockfiles)
(defconst my/temp-files-dir "~/temp/.emacs/" "Directory for backups and auto-saves")
(setq
  backup-by-copying t
  backup-directory-alist `((".*" . ,my/temp-files-dir))
  auto-save-file-name-transforms `((".*" ,my/temp-files-dir t))
)
;(setq create-lockfiles nil)

;; indentation and tabs
(setq electric-indent-mode nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width my/tab-offset)
(setq-default tab-stop-list (number-sequence my/tab-offset (* 2 my/tab-offset) my/tab-offset))

;; line numbers
(global-linum-mode t)
(add-hook 'linum-before-numbering-hook
  (lambda ()
    (setq-local linum-format-fmt
                (let ((w (length (number-to-string
                                  (count-lines (point-min) (point-max))))))
                  (concat "%" (number-to-string w) "d")))))
(defun my/linum-format-func (line)
  (concat
    (propertize (format linum-format-fmt line) 'face 'window-divider-last-pixel)
    (propertize " " 'face 'window-divider-last-pixel)))
(setq linum-format 'my/linum-format-func)

;; columns
(setq column-number-mode t)

;; prompts
(defalias 'yes-or-no-p 'y-or-n-p)
