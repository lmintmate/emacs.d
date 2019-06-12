(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t))
(package-initialize)

(defvar lmintmate/packages '(color-theme-modern
counsel
dired-icon
dired-recent
drag-stuff
emojify
espy
evil
evil-goggles
evil-snipe
free-keys
ivy-rich
no-littering
org-bullets
rainbow-mode
smex
toc-org
transpose-frame
try
undo-tree
vimrc-mode)
  "Core packages")

(setq package-selected-packages lmintmate/packages)

;; Packages for use only on my Linux system

(when (eq system-type 'gnu/linux)
  (add-to-list 'package-selected-packages 'magit))

;; Packages that require emacs 24.4 and up

(unless (version< emacs-version "24.4")
  (add-to-list 'package-selected-packages 'org-cliplink))

(unless (version< emacs-version "24.4")
  (add-to-list 'package-selected-packages 'markdown-mode))

(unless (version< emacs-version "25")
  (add-to-list 'package-selected-packages 'evil-fringe-mark))

(unless (version< emacs-version "25.1")
  (add-to-list 'package-selected-packages 'helpful))

(unless (version< emacs-version "25.2")
  (add-to-list 'package-selected-packages 'minions))

(unless package-archive-contents
  (message "%s" "Refreshing package database...")
  (package-refresh-contents))

(package-install-selected-packages)

;; enforce installing the latest version of org mode
(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))

(add-to-list 'package-selected-packages 'org)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))
(require 'no-littering)

;; set frame
(when (eq system-type 'windows-nt)
(setq default-frame-alist '((top . 5) (left . 220) (width . 80) (height . 30))))

(tool-bar-mode -1)

(if (eq system-type 'windows-nt)
(if (member "DejaVu Sans Mono" (font-family-list))
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140)
(set-face-attribute 'default nil :family "Consolas" :height 140))
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140))

(load-theme 'blue-mood t t)
(enable-theme 'blue-mood)

(set-face-attribute 'fringe nil :background "DodgerBlue4")
(set-face-attribute 'font-lock-negation-char-face nil :foreground "tomato")
(set-face-attribute 'font-lock-doc-face nil :foreground "cyan" :inherit 'unspecified)
(set-face-attribute 'highlight nil :background "#235c94")
(set-face-attribute 'package-status-built-in nil :inherit font-lock-comment-face)
(set-face-attribute 'package-status-dependency nil :inherit font-lock-builtin-face)
(set-face-attribute 'package-status-installed nil :inherit font-lock-function-name-face)
(set-face-attribute 'vc-edited-state nil :background "tomato1" :foreground "black" :box '(:line-width 2 :color "tomato1"))
;; setting so that hl-line-mode won't affect syntax coloring
(set-face-foreground 'highlight nil)

(set-face-attribute 'mode-line nil :background "grey75" :foreground "black" :box '(:line-width 2 :color "grey75"))
(set-face-attribute 'mode-line-inactive nil :background "grey30" :foreground "grey80" :box '(:line-width 2 :color "grey30"))
(set-face-attribute 'mode-line-highlight nil :box '(:line-width 1 :color "grey20"))
(set-face-attribute 'mode-line-buffer-id nil :weight 'normal)

(set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background))

(setq lisp-directory (concat user-emacs-directory "lisp"))

(when (or (not (file-exists-p (expand-file-name "greek.el" lisp-directory))) (not (file-exists-p (expand-file-name "lacarte.el" lisp-directory))))
  (unless (file-directory-p lisp-directory) (make-directory lisp-directory))
  (unless (file-exists-p (expand-file-name "greek.el" lisp-directory))
    (url-copy-file "http://myria.math.aegean.gr/~atsol/emacs-unicode/greek.el" (expand-file-name "greek.el" lisp-directory)))
  (unless (file-exists-p (expand-file-name "lacarte.el" lisp-directory))
    (url-copy-file "https://www.emacswiki.org/emacs/download/lacarte.el" (expand-file-name "lacarte.el" lisp-directory))))

(add-to-list 'load-path lisp-directory)

(load "greek")

(require 'lacarte)
(global-set-key (kbd "\C-c.") 'lacarte-execute-menu-command)

(setq inhibit-startup-screen t)

(setq initial-scratch-message nil)

(defun display-startup-echo-area-message ()
  (message "Καλωσήλθες!"))

(setq calendar-week-start-day 1
          calendar-day-name-array ["Κυριακή" "Δευτέρα" "Τρίτη" "Τετάρτη"
                                   "Πέμπτη" "Παρασκευή" "Σάββατο"]
          calendar-month-name-array ["Ιανουάριος" "Φεβρουάριος" "Μάρτιος"
                                     "Απρίλιος" "Μάιος" "Ιούνιος"
                                     "Ιούλιος" "Αύγουστος" "Σεπτέμβριος"
                                     "Οκτώβριος" "Νοέμβριος" "Δεκέμβριος"])

(setq default-input-method "el_GR")

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'el_GR)

(setq evil-want-C-u-scroll t)

(setq-default evil-auto-indent nil)

(setq evil-toggle-key "C-'")

(setq evil-kill-on-visual-paste nil)

(setq evil-want-fine-undo t)

(setq evil-mode-line-format '(before . mode-line-front-space))

(setq evil-normal-state-tag   (propertize " NORMAL " 'face '((:background "#4f94cd" :foreground "black" :box (:line-width 2 :color "#4f94cd"))))
      evil-emacs-state-tag    (propertize " EMACS " 'face '((:background "MediumPurple2"       :foreground "black" :box (:line-width 2 :color "MediumPurple2"))))
      evil-insert-state-tag   (propertize " INSERT " 'face '((:background "#7fff00"    :foreground "black" :box (:line-width 2 :color "#7fff00"))))
      evil-replace-state-tag  (propertize " REPLACE " 'face '((:background "#ff6347"      :foreground "black" :box (:line-width 2 :color "#ff6347"))))
      evil-motion-state-tag   (propertize " MOTION " 'face '((:background "plum3"          :foreground "black" :box (:line-width 2 :color "plum3"))))
      evil-visual-state-tag   (propertize " VISUAL " 'face '((:background "#ffd700"           :foreground "black" :box (:line-width 2 :color "#ffd700"))))
      evil-operator-state-tag (propertize " OPERATOR " 'face '((:background "yellow"    :foreground "red" :box (:line-width 2 :color "yellow")))))

(require 'evil)
(evil-mode 1)

(evil-set-initial-state 'free-keys-mode 'emacs)

(evil-set-initial-state 'ibuffer-mode 'normal)

(define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "<up>") 'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "<down>") 'evil-next-visual-line)

(define-key evil-normal-state-map (kbd "Q") 'evil-beginning-of-visual-line)
(define-key evil-normal-state-map (kbd "U") 'evil-end-of-visual-line)
(define-key evil-visual-state-map (kbd "Q") 'evil-beginning-of-visual-line)
(define-key evil-visual-state-map (kbd "U") 'evil-end-of-visual-line)

(evil-define-key 'normal text-mode-map
(kbd "<return>") 'newline)

(evil-define-key 'normal prog-mode-map
(kbd "<return>") 'newline)

(define-key evil-normal-state-map (kbd "x") 'delete-forward-char)
(define-key evil-normal-state-map (kbd "X") 'delete-backward-char)

(setq evil-goggles-enable-record-macro nil)

(evil-goggles-mode)

(setq evil-goggles-duration 0.605)

(setq evil-goggles-blocking-duration 0.150)

(set-face-attribute 'evil-goggles-default-face nil :background "#4f94cd" :inherit 'unspecified)

(set-face-attribute 'evil-goggles-delete-face nil :background "tomato" :inherit 'unspecified)
(set-face-attribute 'evil-goggles-change-face nil :background "tomato" :inherit 'unspecified)

(require 'evil-fringe-mark)
(global-evil-fringe-mark-mode)

(set-face-attribute 'evil-fringe-mark-local-face nil :inherit font-lock-function-name-face)

(require 'evil-snipe)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1)

(evil-define-key 'normal evil-snipe-local-mode-map
  "S" nil)

(setq ring-bell-function 'ignore)

(setq custom-file (concat user-emacs-directory "custom.el"))

(setq frame-title-format "%b - Emacs")

(setq display-time-default-load-average nil)
(setq display-time-format "%a %d/%m %H:%M")
(display-time-mode 1)

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq create-lockfiles nil)

(setq delete-by-moving-to-trash t)

(delete-selection-mode 1)

(setq sentence-end-double-space nil)

(unless (display-graphic-p)
  (menu-bar-mode -1))

(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

(global-set-key [f10] 'toggle-scroll-bar)

(global-set-key [f8] 'toggle-frame-maximized)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(add-hook 'find-file-hook
          (lambda ()
            (when (string= (buffer-name) "onetab.txt")
              (visual-line-mode -1))))

(add-hook 'find-file-hook
          (lambda ()
            (when (string= (file-name-extension buffer-file-name) "txt")
              (goto-address-mode 1))))

(define-key global-map "\M-o" 'other-window)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq windmove-wrap-around t)

(defun mark-line (&optional arg)
  (interactive "p")
  (if (not mark-active)
      (progn
        (beginning-of-line)
        (push-mark)
        (setq mark-active t)))
  (forward-line arg))

(define-key evil-emacs-state-map "\C-z" 'mark-line)

(defun lmintmate/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|CURRENTLY\\|SOMEDAY\\|CANCELLED\\|HACK\\|REFACTOR\\|NOCOMMIT\\|LONGTERM\\)"
          1 font-lock-builtin-face t))))

(add-hook 'prog-mode-hook 'lmintmate/add-watchwords)

(when (version<= "26.0.50" emacs-version )
(setq mouse-drag-and-drop-region t))

(define-key evil-emacs-state-map "\C-cz" 'zap-up-to-char)

(setq-default display-line-numbers nil)

(defun noct:relative ()
  (setq-local display-line-numbers 'relative))

(defun noct:line-number-relative ()
  (setq-local display-line-numbers-current-absolute nil))

(when (version<= "26.0.50" emacs-version )
(add-hook 'text-mode-hook #'noct:relative)
(add-hook 'text-mode-hook #'noct:line-number-relative)
(add-hook 'prog-mode-hook #'noct:relative)
(add-hook 'prog-mode-hook #'noct:line-number-relative))

(when (version<= "26.0.50" emacs-version )
(add-hook 'lisp-interaction-mode-hook (lambda () (display-line-numbers-mode -1))))

(when (version<= "26.0.50" emacs-version )
(with-eval-after-load 'display-line-numbers
(set-face-attribute 'line-number-current-line nil :inherit font-lock-comment-face)))

(setq-default help-window-select t)

(evil-set-initial-state 'Info-mode 'emacs)

(define-key Info-mode-map (kbd "q") nil)

(global-set-key (kbd "C-x r d") 'bookmark-delete)

(require 'recentf)
(recentf-mode 1)

(setq recentf-exclude '(".*-autoloads\\.el\\'"
                        "[/\\]\\elpa/"
                        "bookmark"
                        ))

(define-key global-map "\M-]" 'transpose-frame)
(define-key global-map "\M-[" 'rotate-frame)

(require 'ibuffer)
 (global-set-key (kbd "C-x C-b") 'ibuffer)
    (autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Dired" (mode . dired-mode))
	       ("Org" (name . "^.*org$"))
               ("Text" (name . "^.*txt$"))
               ("Markdown" (name . "^.*md$"))

	       ("Emacs Lisp" (mode . emacs-lisp-mode))
	       ("Emacs-created"
                  (or
                   (name . "^\\*")))
	       ))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
  (setq ibuffer-formats
	'((mark modified read-only " "
		(name 18 18 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" "
		filename-and-process)))

(add-hook 'dired-mode-hook 'dired-icon-mode)

(setq dired-icon-image-size 32)

(dired-recent-mode 1)

(evil-define-key 'normal dired-mode-map
"G" 'evil-goto-line
"gg" 'evil-goto-first-line)

(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq auto-revert-verbose nil)

(setq dired-listing-switches "-alh --group-directories-first")

(add-hook 'dired-mode-hook 'dired-sort-toggle-or-edit)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq dired-dwim-target t)

(add-hook 'after-init-hook #'global-emojify-mode)
(setq emojify-emoji-set "twemoji-v2-22")

(global-emojify-mode-line-mode)
(setq-default mode-line-format
      '((:eval (format-mode-line '("%e" evil-mode-line-tag mode-line-front-space (current-input-method (emojify string "🇬🇷")) " " "%Z" " " mode-line-buffer-identification " " (:eval
    (cond (buffer-read-only
           (emojify-string "🔒"))
          ((buffer-modified-p)
           (emojify-string "🖊"))
	  (t "  ")))
 " " "L%l" " " (vc-mode vc-mode) " " mode-line-modes mode-line-misc-info mode-line-end-spaces)))))

(require 'org)

(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-mode)
  (warn "toc-org not found"))

(require 'org-mouse)

(when (package-installed-p 'org-cliplink)
(define-key org-mode-map (kbd "\C-cl") 'org-cliplink))

(define-key org-mode-map (kbd "\C-cd") 'org-toggle-link-display)

(define-key org-mode-map (kbd "\C-ce") 'org-emphasize)

(define-key org-mode-map (kbd "\C-c.") nil)

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)
(if (eq system-type 'windows-nt)
(setq org-bullets-bullet-list
      '("➊" "➋" "➌"))
(setq org-bullets-bullet-list
      '("🅐" "🅑" "🅒")))
(when (eq system-type 'windows-nt)
(setq inhibit-compacting-font-caches t))

(setq org-ellipsis "↪")
(set-face-attribute 'org-ellipsis nil :foreground "cyan3" :underline 'unspecified)

(setq org-todo-keywords
   (quote
    ((sequence "TODO(t)" "⏳ CURRENTLY(c)" "⏲ SOMEDAY(s)" "✘ CANCELLED(x)" "✔ DONE(d)"))))

(setq org-special-ctrl-a/e t)

(setq org-ctrl-k-protect-subtree t)

;; Make windmove work in org-mode:
          (add-hook 'org-shiftup-final-hook 'windmove-up)
          (add-hook 'org-shiftleft-final-hook 'windmove-left)
          (add-hook 'org-shiftdown-final-hook 'windmove-down)
          (add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-footnote-auto-adjust t)

(setq org-return-follows-link t)

(setq org-fontify-done-headline t)
(set-face-attribute 'org-done nil :foreground "PaleGreen" :strike-through t :weight 'bold)
(set-face-attribute 'org-headline-done nil :foreground "LightSalmon" :strike-through t)

(setq org-level-color-stars-only t)

(set-face-attribute 'org-level-2 nil :foreground "gold" :weight 'bold :inherit 'unspecified)
(set-face-attribute 'org-level-3 nil :foreground "cyan3" :weight 'bold :inherit 'unspecified)

(set-face-attribute 'org-block nil :foreground "whitesmoke" :inherit 'unspecified)

(defun my-org-html-postamble (plist)
 (format "Last update : %s" (format-time-string "%a %d/%m/%Y")))
(setq org-html-postamble 'my-org-html-postamble)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("o" . "src org"))
(add-to-list 'org-tempo-keywords-alist '("t" . "title"))

(setq org-use-speed-commands
        (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))

(require 'undo-tree)

(global-undo-tree-mode)

(define-key evil-emacs-state-map "\M-p" 'undo-tree-undo)
(define-key evil-emacs-state-map "\M-n" 'undo-tree-redo)

(require 'drag-stuff)

(add-hook 'text-mode-hook 'drag-stuff-mode)
(add-hook 'prog-mode-hook 'drag-stuff-mode)

(drag-stuff-define-keys)

(require 'espy)
(setq espy-password-file "~/Λήψεις/σημαντικά αρχεία txt/passwords.org")
(setq espy-pass-prefix "password:")

(require 'smex) ; Not needed if you use package.el
  (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                    ; when Smex is auto-initialized on its first run.

(ivy-mode 1)

(setq ivy-use-virtual-buffers t)

(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "\C-cu") 'counsel-unicode-char)
(global-set-key (kbd "\C-cr") 'counsel-recentf)
(global-set-key (kbd "\C-h v") 'counsel-describe-variable)
(global-set-key (kbd "\C-h f") 'counsel-describe-function)

(setq ivy-wrap t)

(setq ivy-initial-inputs-alist nil)

(setq ivy-virtual-abbreviate 'abbreviate)

(setq ivy-extra-directories nil)

(add-to-list 'ivy-format-functions-alist '(t . ivy-format-function-line))

(delete '(t . ivy-format-function-default) ivy-format-functions-alist)

(require 'ivy-rich)
(ivy-rich-mode 1)

(when (package-installed-p 'ivy)
(set-face-attribute 'ivy-org nil :inherit font-lock-function-name-face))

(evil-set-initial-state 'helpful-mode 'motion)
(evil-set-initial-state 'elisp-refs-mode 'motion)

(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

(global-set-key (kbd "C-h k") #'helpful-key)

(add-to-list 'ibuffer-help-buffer-modes 'helpful-mode)

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

(add-hook 'term-mode-hook (lambda ()
      (define-key term-raw-map (kbd "M-x") 'counsel-M-x)
))

(when (package-installed-p 'magit)
(with-eval-after-load 'magit
(set-face-attribute 'magit-diff-context-highlight nil :background "DodgerBlue4" :foreground "grey70")
(set-face-attribute 'magit-section-highlight nil :background "#235c94")
(set-face-attribute 'magit-branch-local nil :foreground "cyan")
(set-face-attribute 'magit-branch-remote nil :foreground "chartreuse")
(set-face-attribute 'magit-section-heading nil :foreground "gold" :weight 'bold)))

(when (package-installed-p 'magit)
(global-set-key (kbd "C-x g") 'magit-status))

(when (package-installed-p 'magit)
(with-eval-after-load 'magit
(define-key with-editor-mode-map (kbd "C-c e") 'emojify-insert-emoji)))

(setq show-paren-delay 0)
(show-paren-mode 1)
(setq show-paren-style (quote mixed))

(electric-pair-mode 1)

(defvar my-electric-pair-modes '(emacs-lisp-mode lisp-interaction-mode))

(defun my-inhibit-electric-pair-mode (char)
  (not (member major-mode my-electric-pair-modes)))

(setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)

(add-hook 'prog-mode-hook 'hs-minor-mode)

(evil-define-key '(normal emacs) prog-mode-map
  (kbd "TAB") 'hs-toggle-hiding
  (kbd "<backtab>") 'hs-hide-all
  (kbd "<C-tab>") 'hs-show-all)

(minions-mode 1)
(setq minions-mode-line-lighter "≡")

(setq ediff-split-window-function (quote split-window-horizontally))

(setq ediff-window-setup-function (quote ediff-setup-windows-plain))

(require 'dired-aux)
;; -*- lexical-binding: t -*-
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2)))
      (error "no more than 2 files should be marked"))))

(define-key dired-mode-map "e" 'ora-ediff-files)

(add-to-list 'safe-local-variable-values
             '(eval add-hook 'after-save-hook
	                (lambda () (org-babel-tangle))
	                nil t))
