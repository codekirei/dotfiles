;; vars
(defvar my/tab-offset 2 "Number of spaces that equal one tab width")
(defvar my/config-dir "config"
  "Dir of config files inside user-emacs-directory")
(defvar my/data-dir (getenv "XDG_DATA_HOME")
  "XDG-compliant path to data dir.")

;; funcs
(defun my/evil-god-toggle ()
  "Toggle between evil and god mode."
  (interactive)
  (if (bound-and-true-p evil-mode)
    (progn
      (evil-mode 0)
      (god-local-mode))
    (progn
      (evil-mode)
      (god-local-mode 0))))

(defun my/indent-newline ()
  "Match previous indentation on newline."
  (interactive)
  (let ((saved-column (current-indentation))
        (line (buffer-substring-no-properties
          (line-beginning-position)
          (line-end-position))))
    (if (= (length (s-trim line)) 0) (my/fully-unindent))
    (newline)
    (indent-to saved-column)))

(defun my/pipe-and-replace (b e)
  "Pipe region to shell and replace with stdout"
  (interactive "r")
  (call-process-region b e (read-string "Shell command: ") t t))

(defun my/unindent ()
  "Move line one tab width left"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-char my/tab-offset)))

(defun my/fully-unindent ()
  "Delete all whitespace preceding line or selection."
  ;; clobbers "m" mark
  (interactive)
  (save-excursion
    (if evil-visual-state-local-minor-mode nil
      (evil-visual-line))
    (evil-exit-visual-state)
    (evil-goto-mark 062) ; >
    (evil-last-non-blank)
    (evil-set-marker 109) ; m
    (evil-goto-mark 060) ; <
    (evil-beginning-of-line)
    (evil-visual-char)
    (evil-goto-mark 109) ; m
    (evil-ex-call-command "`<,`>" "delete-whitespace-rectangle" nil)))

(defun my/get-line-count ()
  "Return number of lines in current buffer"
  (interactive)
  (string-to-number (nth 2 (split-string (count-lines-page) " "))))

(defun my/define-keys (keymap vector)
  "Simultaneously assign multiple keys to a specific keymap."
  (mapc
    (lambda (pair)
      (define-key keymap (kbd (aref pair 0)) (aref pair 1)))
    vector))

(defun my/load-dir (dir)
  "Recursively load all .el files in dir"
  (dolist (el (directory-files-and-attributes dir nil nil nil))
    (let* ((path (car el))
           (fullpath (concat dir "/" path))
           (isdir (car (cdr el)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (my/load-dir fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))
