;; turn off menu bar
(menu-bar-mode -1)

;; diminish server status
(diminish 'server-buffer-clients)

;; utf-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; scroll if close to margin
(setq scroll-margin 10)
(setq scroll-step 1)

;; truncate lines instead of wrapping
(set-default 'truncate-lines t)
(set-display-table-slot standard-display-table 'truncation ?˃)

;; change vertical border glyph
(set-display-table-slot standard-display-table 'vertical-border ?┃)

;; change selective display glyph
(set-display-table-slot standard-display-table
  'selective-display (string-to-vector " ▼"))

;; \n at end of file
(setq require-final-newline t)

;; whitespace
(global-whitespace-mode)
(setq whitespace-style '(face lines-tail tabs trailing empty))
(diminish 'global-whitespace-mode)
(set-face-background 'whitespace-empty "red")
(set-face-background 'whitespace-line "red")
(set-face-background 'whitespace-trailing "red")
(set-face-foreground 'whitespace-line "black")

;; parens
(show-paren-mode 1)
(setq show-paren-style 'expression)
(setq show-paren-delay 0)
(electric-pair-mode 1)
(set-face-background 'show-paren-match "green")
(set-face-foreground 'show-paren-match "black")
(set-face-background 'show-paren-mismatch "red")

;; visual region
(set-face-background 'region "blue")
(set-face-foreground 'region "black")

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

;; prompts
(defalias 'yes-or-no-p 'y-or-n-p)
