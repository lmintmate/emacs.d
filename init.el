(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar lmintmate/packages '(2048-game
anzu
basic-mode
bongo
color-theme-modern
dim
diminish
dired-icon
drag-stuff
emmet-mode
esup
focus
free-keys
geiser
gitignore-mode
google-translate
htmlize
ido-completing-read+
ido-yes-or-no
iedit
isend-mode
legalese
litable
magit
markdown-mode
minimap
ob-translate
olivetti
org
org-cliplink
ox-pandoc
ox-twbs
ox-tufte
pcre2el
pdf-tools
picpocket
racket-mode
smex
transpose-frame
typo
undo-tree
w3m
wttrin
yaml-mode
zeal-at-point)
  "Default packages")

(defun lmintmate/packages-installed-p ()
  (loop for pkg in lmintmate/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (lmintmate/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg lmintmate/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(require 'org)
(org-babel-load-file "~/.emacs.d/README.org")
