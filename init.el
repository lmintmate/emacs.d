(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
;; disables tool-bar-mode. I put it this up so that the tool-bar won't be loaded and disabled afterwards, but be disabled from the get-go.
(tool-bar-mode -1)
;; Setting the font size to 14
(set-face-attribute 'default nil :height 140)
;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; personal directory parameters - not included in the version control, for obvious reasons. The list of relevant parameters is however included in personal-parameters.txt.
(load-file "~/.emacs.d/personal-parameters.el")

;; nationality parameters - not included in the version control, for obvious reasons. The list of relevant parameters is however included in nationality-parameters.txt.
(load-file "~/.emacs.d/nationality-parameters.el")

;; settings for multiple buffer management
(load-file "~/.emacs.d/buffer-defuns.el")

;; newsticker configuration - not included in the version control
;; (btw, newsticker is awesome, it's just like Liferea, but inside emacs!)
(load-file "~/.emacs.d/newsticker-config.el")

;; other config parameters
(setq auto-save-default nil)
;; emacs-w3m = default browser from inside emacs
;;(setq browse-url-browser-function (quote w3m-browse-url))
(setq delete-by-moving-to-trash t)
(setq delete-selection-mode t)
(setq geiser-active-implementations (quote (guile racket chez mit chibi)))
(setq make-backup-files nil)
(setq org-todo-keywords
   (quote
    ((sequence "TODO(t)" "CURRENTLY(c)" "SOMEDAY(s)" "DONE(d)"))))
(setq racket-memory-limit 128)
(setq remember-notes-initial-major-mode (quote text-mode))
(setq w3m-default-display-inline-images t)
;; Just in case I'll want to disable the menu bar as well. The content of the menus can still be accessed as a popup menu by using C-mouse-3(ctrl+right mouse click)
;; (menu-bar-mode -1)
;; disabling menu bar when emacs is run in terminal (since it can't be clicked anyways, it takes up space without reason...)
(when (not (window-system))
  (menu-bar-mode -1))
;; Toggling the menu bar with a keyboard shortcut
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)
;; toggling the scroll bar with a keyboard shortcut
(global-set-key [f10] 'toggle-scroll-bar)
;; Set input method to greek to be able to write greek with keyboard set to English (useful for those pesky Latin C- and M- shortcuts). Toggle with C-\
(set-input-method "greek")

;; new message for startup echo area
(defun display-startup-echo-area-message ()
  (message "Καλωσήλθες!"))

;; visual line mode only for text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; associate .txt files with goto-address-mode, that highlights urls and makes them clickable
(add-hook 'find-file-hook
          (lambda ()
            (when (string= (file-name-extension buffer-file-name) "txt")
              (goto-address-mode 1))))

;; adds melpa repository
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; load manually installed packages.
;; loads my personalized malyon package
(load "malyon") ;; best not to include the ending “.el” or “.elc”
(load "web-search")

;; Adds shift + arrows for changing buffer, in addition to Ctrl+O
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; pdf tools install: uncomment for right after installing pdf tools, comment again afterwards, as to not delay emacs loading time, and uncomment again if need to open pdf from emacs arises
(pdf-tools-install)

;; blue mood theme
(load-theme 'blue-mood t t)
(enable-theme 'blue-mood)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "DodgerBlue4")))))

;; enable dired icon mode
(add-hook 'dired-mode-hook 'dired-icon-mode)

;; load mpg123
(autoload 'mpg123 "mpg123" "A Front-end to mpg123/ogg123" t)

;; deft - quick note taking
(with-eval-after-load 'deft)
(setq deft-extensions '("txt" "md" "org"))
(setq deft-time-format " %d-%m-%Y %H:%M")
;; default mode for deft - switch between the 2 below
;;(setq deft-default-extension "org")
(setq deft-default-extension "md")

;;orgmode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
;; org now supports shift selection except in special instances
(setq org-support-shift-select t)
;; ox-tufte: package that exports with the prettier tufte css
(require 'ox-tufte)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode)
(define-key global-map "\M-'" 'undo-tree-redo)

;; olivetti mode
(setq olivetti-hide-mode-line t)
;; function to turn off menu bar when olivetti mode is enabled
(progn
  (defun turn-off-menu-with-olivetti ()
    (menu-bar-mode -1))
  (add-hook 'olivetti-mode-hook 'turn-off-menu-with-olivetti))

;; web-search default provider
(setq web-search-default-provider "DuckDuckGo")

;; to load nov.el epub reader
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; emms setup (for soundklaus)
(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; drag-stuff
(require 'drag-stuff)
(add-hook 'text-mode-hook 'drag-stuff-mode)
(drag-stuff-define-keys)

;; ido mode
(require 'ido)
(ido-mode)
(ido-everywhere)
;; prefix matching - will only display results that start with your selection
(setq ido-enable-prefix t)

;;smex
(require 'smex) ; Not needed if you use package.el
  (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                    ; when Smex is auto-initialized on its first run.
;; keyboard shortcuts
  (global-set-key (kbd "M-x") 'smex)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setq smex-flex-matching nil)
