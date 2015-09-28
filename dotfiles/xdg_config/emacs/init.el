;; read XDG dirs
(defvar my/config-dir (concat (getenv "XDG_CONFIG_HOME") "/emacs/")
  "XDG-compliant path to emacs config dir.")

(defvar my/data-dir (getenv "XDG_DATA_HOME")
  "XDG-compliant path to data dir.")

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
