;; vars
(defvar my/tab-offset 2 "Number of spaces that equal one tab width")
(defvar my/config-dir "config" "Dir of config files inside user-emacs-directory")

;; funcs
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
