(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t))
(package-initialize)

(defvar lmintmate/packages '(bongo
color-theme-modern
counsel
diminish
dired-icon
drag-stuff
emmet-mode
esup
esxml
free-keys
htmlize
malyon
org
rainbow-mode
smex
transpose-frame
typo
undo-tree
w3m
web-search)
  "Core packages")

(unless package-archive-contents
  (message "%s" "Refreshing package database...")
  (package-refresh-contents))
  (dolist (pkg lmintmate/packages)
    (unless (package-installed-p pkg)
      (package-install pkg)))

;; Conditionals for installing of certain packages (idea from https://github.com/pgdouyon/dotfiles/blob/master/emacs)

;; Packages for use only on my Linux system

(when (eq system-type 'gnu/linux)
(unless (package-installed-p 'basic-mode)
  (package-install 'basic-mode)))

(when (eq system-type 'gnu/linux)
(unless (package-installed-p 'isend-mode)
  (package-install 'isend-mode)))

(when (eq system-type 'gnu/linux)
(unless (package-installed-p 'magit)
  (package-install 'magit)))

(when (eq system-type 'gnu/linux)
(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools)))

(when (eq system-type 'gnu/linux)
(unless (package-installed-p 'geiser)
  (package-install 'geiser)))

(when (eq system-type 'gnu/linux)
(unless (package-installed-p 'racket-mode)
  (package-install 'racket-mode)))

(when (eq system-type 'gnu/linux)
(unless (package-installed-p 'zeal-at-point)
  (package-install 'zeal-at-point)))

(when (eq system-type 'gnu/linux)
(unless (package-installed-p 'ox-pandoc)
  (package-install 'ox-pandoc)))

;; Packages that require emacs 24.4 and up

(unless (version< emacs-version "24.4")
(unless (package-installed-p 'org-cliplink)
  (package-install 'org-cliplink)))

(unless (version< emacs-version "24.4")
(unless (package-installed-p 'wttrin)
  (package-install 'wttrin)))

(unless (version< emacs-version "24.4")
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode)))

(unless (version< emacs-version "24.4")
(unless (package-installed-p 'dim)
  (package-install 'dim)))

(unless (version< emacs-version "24.4")
(unless (package-installed-p 'olivetti)
  (package-install 'olivetti)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'org)
(org-babel-load-file (concat user-emacs-directory "README.org"))
