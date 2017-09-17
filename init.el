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

;; personal directory parameters
(setq deft-directory "~/.emacs.d/deft")
(setq diary-file "~/.emacs.d/diary")
(setq geiser-racket-binary "~/racket/bin/racket")
(setq malyon-stories-directory "~/other-games/frotz-games")
(setq racket-program "~/racket/bin/racket")
(defvar mpg123-default-dir "~/mousiki-gia-emacs")
(setq bongo-default-directory "~/Μουσική")

;; nationality parameters
;; so that wttrin will show correct cities and language
(setq wttrin-default-cities (quote ("Nicosia" "Chania")))
(setq wttrin-default-accept-language '("Accept-Language" . "el-GR"))
;; greek calendar
(setq calendar-week-start-day 1
          calendar-day-name-array ["Κυριακή" "Δευτέρα" "Τρίτη" "Τετάρτη"
                                   "Πέμπτη" "Παρασκευή" "Σάββατο"]
          calendar-month-name-array ["Ιανουάριος" "Φεβρουάριος" "Μάρτιος"
                                     "Απρίλιος" "Μάιος" "Ιούνιος"
                                     "Ιούλιος" "Αύγουστος" "Σεπτέμβριος"
                                     "Οκτώβριος" "Νοέμβριος" "Δεκέμβριος"])

;; settings for multiple buffer management
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
;; flips 2 window frame, so that left goes right, and up goes down
;; (from http://whattheemacsd.com/buffer-defuns.el-02.html)
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))
;; the keyboard shortcut for the above function
(define-key global-map "\M-[" 'rotate-windows)

;; newsticker configuration
;; (btw, newsticker is awesome, it's just like Liferea, but inside emacs!)
;; keep none of the proposed by emacs urls in the list
(setq newsticker-url-list-defaults nil)
;; do not keep obsolete items
(setq newsticker-keep-obsolete-items nil)
;; newsticker's url list. Is automatically populated with M-x newsticker-opml-import.
(setq newsticker-url-list
   (quote
    (("xkcd.com" "http://xkcd.com/rss.xml" nil nil nil)
     ("Opensource.com" "https://opensource.com/feed" nil nil nil)
     ("Awful Library Books" "http://feeds.feedburner.com/awfullibrarybooks?format=xml" nil nil nil)
     ("OmgUbuntu" "http://feeds.feedburner.com/d0od" nil nil nil)
     ("Reddit Linux" "https://www.reddit.com/r/linux/.rss" nil nil nil)
     ("Reddit Linux Mint" "https://www.reddit.com/r/linuxmint/.rss" nil nil nil)
     ("Reddit linuxmasterrace" "https://www.reddit.com/r/linuxmasterrace/.rss" nil nil nil))))

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

;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")
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
;; bigger icons
(setq dired-icon-image-size 32)
;; other dired parameters
(setq dired-listing-switches "-al --group-directories-first")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; to sort files by modified date
(add-hook 'dired-mode-hook 'dired-sort-toggle-or-edit)

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
(define-key global-map "\M-/" 'undo-tree-redo)

;; olivetti mode
(setq olivetti-hide-mode-line t)
;; function to turn off menu bar when olivetti mode is enabled
(progn
  (defun turn-off-menu-with-olivetti ()
    (menu-bar-mode -1))
  (add-hook 'olivetti-mode-hook 'turn-off-menu-with-olivetti))

;; web-search default provider
(setq web-search-default-provider "DuckDuckGo")

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
;; ido completing-read+
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
;; ido yes-or-no
(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)

;;smex
(require 'smex) ; Not needed if you use package.el
  (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                    ; when Smex is auto-initialized on its first run.
;; keyboard shortcuts
  (global-set-key (kbd "M-x") 'smex)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
