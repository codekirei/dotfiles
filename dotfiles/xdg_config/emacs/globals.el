;; vars
(defvar my/tab-offset 2 "Number of spaces that equal one tab width")
(defvar my/config-dir "config"
  "Dir of config files inside user-emacs-directory")
(defvar my/data-dir (getenv "XDG_DATA_HOME")
  "XDG-compliant path to data dir.")

;; funcs
(defun my/what-column ()
  "Return column of pointer."
  (interactive)
  (string-to-number (car (last (split-string
    (car (last (split-string (what-cursor-position)))) "=")))))

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

(defun my/trimmed-line-to-point ()
  "Returns contents of current line to point with trimmed whitespace."
  (interactive)
  (s-trim (buffer-substring (line-beginning-position)
                            (point))))

(defun my/indent-newline ()
  "Match previous indentation on newline."
  (interactive)
  (let ((saved-column (current-indentation)))
    (if (= 0 (length (my/trimmed-line-to-point)))
      (my/fully-unindent-line))
    (newline)
    (indent-to saved-column)))

(defun my/pipe-and-replace (b e)
  "Pipe region to shell and replace with stdout"
  (interactive "r")
  (call-process-region b e (read-string "Shell command: ") t t))

(defun my/fully-unindent-line ()
  "Delete all whitespace preceding line."
  (interactive)
  (goto-char (line-beginning-position))
  (while (and (char-after (point))
    (char-equal (string-to-char "\s") (char-after (point))))
      (delete-forward-char 1)))

(defun my/fully-unindent ()
  "Delete all whitespace preceding line or region."
  (interactive)
  (if (region-active-p)
    ;; then
    (funcall (lambda () (interactive)
      (let ((b (region-beginning))
            (e (region-end)))
      (if evil-visual-state-minor-mode (evil-exit-visual-state))
      (save-excursion
        (goto-char (- e 1))
        (while (>= (point) b)
          (my/fully-unindent-line)
          (previous-line))))))
    ;; else
    (my/fully-unindent-line)))

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
