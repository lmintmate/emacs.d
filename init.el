;; adds melpa repository
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(require 'org)
(org-babel-load-file "~/.emacs.d/config.org")
