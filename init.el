(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t))
(package-initialize)

(defvar lmintmate/packages '(basic-mode
bongo
color-theme-modern
counsel
dim
diminish
dired-icon
drag-stuff
emmet-mode
esup
esxml
focus
free-keys
geiser
htmlize
iedit
isend-mode
magit
malyon
markdown-mode
olivetti
org
org-cliplink
ox-pandoc
pdf-tools
racket-mode
rainbow-mode
smex
transpose-frame
typo
undo-tree
w3m
web-search
wttrin
zeal-at-point)
  "Default packages")

(require 'cl-lib)
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
(org-babel-load-file (concat user-emacs-directory "README.org"))
