(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
;; Setting the font size to 14
(set-face-attribute 'default nil :height 140)
;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; fancy splash screen bug (see https://emacs.stackexchange.com/questions/20976/x11-why-is-the-emacs-logo-image-missing-on-the-welcome-screen)
(defun use-fancy-splash-screens-p ()
      "Return t if fancy splash screens should be used."
      (when (and (display-graphic-p)
                 (or (and (display-color-p)
              (image-type-available-p 'xpm))
                     (image-type-available-p 'pbm)))
        (let ((frame (fancy-splash-frame)))
          (when frame
      (let* ((img (create-image (fancy-splash-image-file)))
             (image-height (and img (cdr (image-size img nil frame))))
             ;; We test frame-height so that, if the frame is split
             ;; by displaying a warning, that doesn't cause the normal
             ;; splash screen to be used.
             (frame-height (1- (frame-height frame))))
       ;; The original value added to the `image-height' for the test was 19; however,
       ;; that causes the test to fail on X11 by about 1.5 -- so use 17 instead.
        (> frame-height (+ image-height 17)))))))

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
;; disables tool-bar-mode
(tool-bar-mode -1)
;; Just in case I'll want to disable the menu bar as well. The content of the menus can still be accessed as a popup menu by using C-mouse-3(ctrl+right mouse click)
;; (menu-bar-mode -1)
;; Toggling the menu bar with a keyboard shortcut
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

;; new message for startup echo area
(defun display-startup-echo-area-message ()
  (message "Καλωσήλθες!"))

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
;; function to turn off menu bar when olivetti mode is enabled
(progn
  (defun turn-off-menu-with-olivetti ()
    (menu-bar-mode -1))
  (add-hook 'olivetti-mode-hook 'turn-off-menu-with-olivetti))

;; change layout of windows from horizontal to vertical very easily
;; (from http://whattheemacsd.com/buffer-defuns.el-03.html)
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
;; the keyboard shortcut for the above function
(define-key global-map "\M-]" 'toggle-window-split)
