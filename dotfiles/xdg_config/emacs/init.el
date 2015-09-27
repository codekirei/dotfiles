;; set absolute path to config dir
(defvar my/config-dir (concat (getenv "XDG_CONFIG_HOME") "/emacs/")
  "Absolute path to emacs config dir.")

;; load globals
(load (concat my/config-dir "globals.el"))

;; set up use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; load rest of modular conf
(my/load-dir (concat my/config-dir "modules"))
