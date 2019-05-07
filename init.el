(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t))
(package-initialize)

(defvar lmintmate/packages '(color-theme-modern
counsel
diminish
dired-icon
drag-stuff
espy
evil
evil-goggles
free-keys
ivy-rich
linum-relative
rainbow-mode
smex
toc-org
transpose-frame
try
undo-tree
vimrc-mode
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
(unless (package-installed-p 'magit)
  (package-install 'magit)))

;; Packages that require emacs 24.4 and up

(unless (version< emacs-version "24.4")
(unless (package-installed-p 'org-cliplink)
  (package-install 'org-cliplink)))

(unless (version< emacs-version "24.4")
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode)))

(unless (version< emacs-version "24.4")
(unless (package-installed-p 'dim)
  (package-install 'dim)))

(unless (version< emacs-version "25")
(unless (package-installed-p 'evil-fringe-mark)
  (package-install 'evil-fringe-mark)))

;; enforce installing the latest version of org mode
(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'org)
(org-babel-load-file (concat user-emacs-directory "README.org"))
