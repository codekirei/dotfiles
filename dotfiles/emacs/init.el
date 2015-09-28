(defvar my/config-dir (concat (getenv "XDG_CONFIG_HOME") "/emacs/")
  "XDG-compliant path to emacs config dir.")
(load (concat my/config-dir "init.el"))
