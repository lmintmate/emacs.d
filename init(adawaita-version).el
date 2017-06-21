(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode t)
 '(geiser-active-implementations (quote (guile racket chez mit chibi)))
 '(geiser-racket-binary "~/racket/bin/racket")
 '(make-backup-files nil)
 '(racket-memory-limit 128)
 '(racket-program "~/racket/bin/racket")
 '(remember-notes-initial-major-mode (quote text-mode))
 '(tool-bar-mode nil))
;; Setting the font size to 14
(set-face-attribute 'default nil :height 140)
;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; visual line mode only for text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; changes highlighting color from white to green
(set-face-attribute 'region nil :background "#92EE9C")
 )

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

;; greek calendar
(setq calendar-week-start-day 1
          calendar-day-name-array ["Κυριακή" "Δευτέρα" "Τρίτη" "Τετάρτη"
                                   "Πέμπτη" "Παρασκευή" "Σάββατο"]
          calendar-month-name-array ["Ιανουάριος" "Φεβρουάριος" "Μάρτιος"
                                     "Απρίλιος" "Μάιος" "Ιούνιος"
                                     "Ιούλιος" "Αύγουστος" "Σεπτέμβριος"
                                     "Οκτώβριος" "Νοέμβριος" "Δεκέμβριος"])

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
(defvar mpg123-default-dir "~/mousiki-gia-emacs")

;; wttrin weather
(setq wttrin-default-cities '("Nicosia" "Cyprus"))
(setq wttrin-default-accept-language '("Accept-Language" . "el-GR"))

;; deft - quick note taking
(require 'deft)
(setq deft-directory "~/.emacs.d/deft")

;; location of diary file
(setq diary-file "~/.emacs.d/diary")
