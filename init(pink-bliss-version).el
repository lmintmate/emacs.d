(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
'(tool-bar-mode nil))
;; Setting the font size to 14
(set-face-attribute 'default nil :height 140)
;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; personal directory parameters - not included in the version control, for obvious reasons. The list of relevant parameters is however included in personal-parameters.txt.
(load-file "~/.emacs.d/personal-parameters.el")

;; nationality parameters - not included in the version control, for obvious reasons. The list of relevant parameters is however included in nationality-parameters.txt.
(load-file "~/.emacs.d/nationality-parameters.el")

;; other config parameters
(setq auto-save-default nil)
(setq cfw:display-calendar-holidays nil)
(setq delete-selection-mode t)
(setq geiser-active-implementations (quote (guile racket chez mit chibi)))
(setq make-backup-files nil)
(setq org-todo-keywords
   (quote
    ((sequence "TODO(t)" "CURRENTLY(c)" "SOMEDAY(s)" "DONE(d)"))))
(setq racket-memory-limit 128)
(setq remember-notes-initial-major-mode (quote text-mode))

;; new message for startup echo area
(defun display-startup-echo-area-message ()
  (message "Καλωσήλθες!"))

;; visual line mode only for text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; adds melpa repository
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; load manually installed packages.
;; loads my personalized malyon package
(load "malyon") ;; best not to include the ending “.el” or “.elc”

;; Adds shift + arrows for changing buffer, in addition to Ctrl+O
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; pdf tools install: uncomment for right after installing pdf tools, comment again afterwards, as to not delay emacs loading time, and uncomment again if need to open pdf from emacs arises
;; (pdf-tools-install)

;; auto-complete geiser
(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

;; enable dired icon mode
(add-hook 'dired-mode-hook 'dired-icon-mode)

;; load mpg123
(autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)

;; deft - quick note taking
(require 'deft)
(setq deft-extensions '("txt" "md" "org"))
(setq deft-time-format " %d-%m-%Y %H:%M")
;; default mode for deft - switch between the 2 below
;;(setq deft-default-extension "org")
(setq deft-default-extension "md")

;; pink-bliss theme
(load-theme 'pink-bliss t t)
(enable-theme 'pink-bliss)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; calfw calendar
;; regular calendar (M-x cfw:open-calendar-buffer)
(require 'calfw)
;; to show diary entries (M-x cfw:open-diary-calendar)
(require 'calfw-cal)

;;orgmode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)
(define-key global-map "\M-'" 'undo-tree-redo)

;; olivetti mode
(setq olivetti-hide-mode-line t)
