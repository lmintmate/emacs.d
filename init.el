;; -*- lexical-binding: t -*-

(when (version< emacs-version "25.1")
  (error "This configuration requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

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
free-keys
no-littering
parent-mode
rainbow-mode
toc-org
transpose-frame
try
undo-tree
vimrc-mode
;; emacs 24.4 and above
elisp-demos
evil-goggles
evil-snipe
ivy-rich
markdown-mode
org-cliplink
;; emacs 25.1 and above
evil-fringe-mark
evil-traces
helpful
ivy-prescient)
  "Core packages")

(setq package-selected-packages lmintmate/packages)

;; Packages to be installed only when a certain executable is on the path

(when (executable-find "git")
  (add-to-list 'package-selected-packages 'magit))

;; Packages for use only on my Linux system

(when (eq system-type 'gnu/linux)
  (add-to-list 'package-selected-packages 'trashed))

;; Packages that require emacs versions above 25.1

(unless (version< emacs-version "25.2")
  (add-to-list 'package-selected-packages 'minions))

;; GNU ELPA keyring package for versions below 26.3
(when (version< emacs-version "26.3" )
  (add-to-list 'package-selected-packages 'gnu-elpa-keyring-update))

(unless package-archive-contents
  (message "%s" "Refreshing package database...")
  (package-refresh-contents))

(when (fboundp 'package-install-selected-packages)
  (package-install-selected-packages))

;; enforce installing the latest version of org mode
(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
(if (yes-or-no-p "Do you want to install the latest version of org-mode?")
  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0))
(message "The latest version of org-mode wasn't installed.")))

(add-to-list 'package-selected-packages 'org)

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                (let ((pkg (cadr (assq name where))))
                  (when pkg
                    (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(when (string= (system-name) "LAPTOP-LHEH01GE")
(setq system-time-locale "C"))

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

(when (eq system-type 'windows-nt)
(set-face-attribute 'fixed-pitch nil :family "Consolas" :height 140))

(load-theme 'blue-mood t t)
(enable-theme 'blue-mood)

(set-face-attribute 'fringe nil :background "DodgerBlue4")
(set-face-attribute 'font-lock-negation-char-face nil :foreground "tomato")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "tomato")
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

(unless (file-directory-p lisp-directory) (make-directory lisp-directory))

;; in addition to greek.el, also download the byte-compiled greek.elc
(unless (file-exists-p (expand-file-name "greek.el" lisp-directory))
    (url-copy-file "http://myria.math.aegean.gr/~atsol/emacs-unicode/greek.el" (expand-file-name "greek.el" lisp-directory)))
(unless (file-exists-p (expand-file-name "greek.elc" lisp-directory))
    (url-copy-file "http://myria.math.aegean.gr/~atsol/emacs-unicode/greek.elc" (expand-file-name "greek.elc" lisp-directory)))

;; byte-compile .el files after downloading them
(unless (file-exists-p (expand-file-name "lacarte.el" lisp-directory))
    (url-copy-file "https://www.emacswiki.org/emacs/download/lacarte.el" (expand-file-name "lacarte.el" lisp-directory)))
(unless (file-exists-p (expand-file-name "lacarte.elc" lisp-directory))
(byte-compile-file (expand-file-name "lacarte.el" lisp-directory)))

(unless (file-exists-p (expand-file-name "org-bullets.el" lisp-directory))
   (url-copy-file "https://raw.githubusercontent.com/lmintmate/org-bullets/invisible-leading-stars/org-bullets.el" (expand-file-name "org-bullets.el" lisp-directory)))
(unless (file-exists-p (expand-file-name "org-bullets.elc" lisp-directory))
(byte-compile-file (expand-file-name "org-bullets.el" lisp-directory)))

(unless (file-exists-p (expand-file-name "elispfl.el" lisp-directory))
   (url-copy-file "https://raw.githubusercontent.com/lmintmate/elispfl/master/elispfl.el" (expand-file-name "elispfl.el" lisp-directory)))
(unless (file-exists-p (expand-file-name "elispfl.elc" lisp-directory))
(byte-compile-file (expand-file-name "elispfl.el" lisp-directory)))

(add-to-list 'load-path lisp-directory)

;; load elisp file, use byte compiled version (.elc) if it exists
(load "greek")

(require 'lacarte)
(global-set-key (kbd "\C-c.") 'lacarte-execute-menu-command)

(require 'elispfl)

(with-eval-after-load 'elisp-mode
  (elispfl-mode))
;; Highlight face name by the face itself
(setq elispfl-face-use-itself t)

(setq inhibit-startup-screen t)

(unless (executable-find "fortune")
(unless (file-exists-p (concat user-emacs-directory "apofthegmata.txt"))
(url-copy-file "https://gitlab.com/snippets/1870200/raw" (concat user-emacs-directory "apofthegmata.txt")))
(require 'cookie1)
(defun lmintmate/cookie-insert (phrase-file &optional count startmsg endmsg)
  (setq phrase-file (cookie-check-file phrase-file))
  (let ((cookie-vector (cookie-snarf phrase-file startmsg endmsg)))
    (cookie-shuffle-vector cookie-vector)
    (let ((start (point)))
      (cookie1 (min (- (length cookie-vector) 1) (or count 1)) cookie-vector)
      (fill-region-as-paragraph start (point) nil)))))

(if (executable-find "fortune")
   (setq initial-scratch-message
         (with-temp-buffer
           (shell-command "fortune" t)
           (let ((comment-start ";;"))
             (comment-region (point-min) (point-max)))
           (concat (buffer-string))))
(if (file-exists-p (concat user-emacs-directory "apofthegmata.txt"))
(setq initial-scratch-message
(with-temp-buffer
           (lmintmate/cookie-insert
(concat user-emacs-directory "apofthegmata.txt"))
           (let ((comment-start ";;"))
             (comment-region (point-min) (point-max)))
           (concat (buffer-string) "\n")))
(setq initial-scratch-message (concat ";; Είς οιωνός άριστος, αμύνεσθαι περί πάτρης." "\n"))))

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

(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

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

(evil-define-key 'normal org-mode-map
(kbd "<return>") 'org-return)

(evil-define-key 'normal prog-mode-map
(kbd "<return>") 'newline)

(define-key evil-normal-state-map (kbd "x") 'delete-forward-char)
(define-key evil-normal-state-map (kbd "X") 'delete-backward-char)

(define-key evil-visual-state-map "ae" 'mark-whole-buffer)

(evil-define-operator my/evil-replace-with-kill-ring (beg end)
    "Replace with killring action."
    :move-point nil
    (interactive "<r>")
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (call-interactively 'evil-paste-before 1)))

  (define-key evil-normal-state-map "gc" 'my/evil-replace-with-kill-ring)

(evil-define-text-object my/function-text-object (count)
  "Function text object"
  (interactive)
  (save-mark-and-excursion
    (mark-defun)
    (let ((m (mark)))
      (if (looking-back "*/\n")
          (progn
            (previous-line)
            (list m (first (sp-get-comment-bounds))))
        (list m (point))))))
(define-key evil-inner-text-objects-map "f" 'my/function-text-object)
(define-key evil-outer-text-objects-map "f" 'my/function-text-object)

(setq evil-goggles-enable-record-macro nil)

(setq evil-goggles-enable-set-marker nil)

(evil-goggles-mode)

(setq evil-goggles-duration 0.605)

(setq evil-goggles-blocking-duration 0.250)

(evil-goggles-use-diff-refine-faces)

(require 'evil-fringe-mark)
(global-evil-fringe-mark-mode)

(set-face-attribute 'evil-fringe-mark-local-face nil :inherit font-lock-function-name-face)

(require 'evil-snipe)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1)

(evil-define-key 'normal evil-snipe-local-mode-map
  "S" nil)

(evil-traces-mode)
(evil-traces-use-diff-refine-faces)

(setq ring-bell-function 'ignore)

(setq use-dialog-box nil)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

(setq frame-title-format
    '((:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                    "%b"))
      (:eval (if (buffer-modified-p)
                 " [+]"))
      " - Emacs " emacs-version))

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

(global-set-key [f8] 'toggle-menu-bar-mode-from-frame)

(set-face-attribute 'window-divider nil :foreground "gray75")
(set-face-attribute 'window-divider-first-pixel nil :foreground "gray95")
(set-face-attribute 'window-divider-last-pixel nil :foreground "gray55")

(add-hook 'window-divider-mode-hook (lambda () (scroll-bar-mode 'toggle)))
(global-set-key [f10] 'window-divider-mode)

(global-set-key [f9] 'toggle-frame-maximized)

(setq frame-inhibit-implied-resize t)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(add-hook 'find-file-hook
          (lambda ()
            (when (string= (buffer-name) "onetab.txt")
              (visual-line-mode -1))))

(add-hook 'find-file-hook
          (lambda ()
            (when (string= (file-name-extension buffer-file-name) "txt")
              (goto-address-mode 1))))

(when (version< emacs-version "26.0.50" )
(defun my-kill-buffer ()
    "Kill current buffer without prompting"
    (interactive)
    (kill-buffer (current-buffer))))

(if (version<= "26.0.50" emacs-version )
(global-set-key "\C-ck" 'kill-current-buffer)
(global-set-key "\C-ck" 'my-kill-buffer))

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

(when (fboundp 'display-line-numbers-mode)
(setq-default display-line-numbers nil)
(defun noct:relative ()
  (setq-local display-line-numbers 'relative))
(defun noct:line-number-relative ()
  (setq-local display-line-numbers-current-absolute nil)))

(when (fboundp 'display-line-numbers-mode)
(add-hook 'text-mode-hook #'noct:relative)
(add-hook 'text-mode-hook #'noct:line-number-relative)
(add-hook 'prog-mode-hook #'noct:relative)
(add-hook 'prog-mode-hook #'noct:line-number-relative))

(when (fboundp 'display-line-numbers-mode)
(add-hook 'lisp-interaction-mode-hook (lambda () (display-line-numbers-mode -1))))

(when (fboundp 'display-line-numbers-mode)
(with-eval-after-load 'display-line-numbers
(set-face-attribute 'line-number-current-line nil :inherit font-lock-comment-face)))

(setq-default help-window-select t)

(setq disabled-command-function nil)

(setq custom-safe-themes t)

(defadvice load-theme (before clear-previous-themes activate)
    "Clear existing theme settings instead of layering them"
    (mapc #'disable-theme custom-enabled-themes))

(evil-set-initial-state 'Info-mode 'emacs)

(define-key Info-mode-map (kbd "q") nil)

(setq custom-unlispify-tag-names nil)

(global-set-key (kbd "C-x r d") 'bookmark-delete)

(defun d/download-file (&optional url name)
  "Download a file from url to specified path."
  (interactive)
  (let* ((file-url (or url (read-from-minibuffer "URL: ")))
         (file-name
          (or name
              (counsel-find-file
               (file-name-nondirectory file-url)))))
    (url-copy-file file-url file-name)))

(defun d/switch-to-scratch ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "\C-cs") 'd/switch-to-scratch)

(defun spacemacs/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun spacemacs/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun lmintmate/rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.
When NEW-FILENAME is not specified, asks user for a new name.
Also renames associated buffers (if any exists) and updates recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((is-dir (file-directory-p filename))
           (short-name
            (if is-dir
                (file-name-base (directory-file-name filename))
              (file-name-nondirectory filename)))
           (new-filename
            (if new-filename new-filename
              (read-file-name
               (format "Rename %s to: " short-name)))))

      ;; Rename filename to new-filename and error if new-filename already
      ;; exists. `dired-rename-file' handles renaming of directories and files.
      ;; It updates the name of all associated buffers.
      (dired-rename-file filename new-filename nil)

      ;; Update recentf list.
      (when (fboundp 'recentf-add-file)
        (seq-map
         (lambda (fp)
           (recentf-add-file
            (concat new-filename (string-remove-prefix filename fp)))
           (recentf-remove-if-non-kept fp))
         (seq-filter
          (lambda (fp)
            (string-prefix-p filename fp))
          recentf-list)))

      ;; Inform user about tremendous success.
      (message "%s '%s' successfully renamed to '%s'"
               (if is-dir "Directory" "File")
               short-name
               (file-name-nondirectory new-filename)))))

(defun lmintmate/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.
If called without a prefix argument, the prompt is
initialized with the current directory instead of filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name)
                               'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?")
                                  new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

(defun my-live-face-color-changer (face)
"Change colors of various faces with live feedback. Copies result to clipboard."
  (interactive (list (read-face-name "Select face"
                                     (or (face-at-point t) 'default)
                                     t)))
  (setq my-live-face (car face))
  (setq my-live-face-foreground t)
  (list-colors-display)
  (select-window (get-buffer-window "*Colors*"))
  (evil-local-set-key 'motion "f" 'my-live-set-foreground)
  (evil-local-set-key 'motion "b" 'my-live-set-background)
  (evil-local-set-key 'motion "c" 'my-live-copy-colors)
  (add-hook 'post-command-hook 'my-live-face-color-set t t))

(defun my-live-face-color-set ()
  (when (looking-at ".+\\(#.+\\)")
    (funcall (if my-live-face-foreground
                 'set-face-foreground
               'set-face-background)
             my-live-face
             (match-string 1))))

(defun my-live-set-foreground ()
  (interactive)
  (setq my-live-face-foreground t)
  (message "Choosing foreground color."))

(defun my-live-set-background ()
  (interactive)
  (setq my-live-face-foreground nil)
  (message "Choosing background color."))

(defun my-live-copy-colors ()
  (interactive)
  (remove-hook 'post-command-hook 'my-live-face-color-set t)
  (let ((settings (format "(set-face-attribute '%s nil :foreground \"%s\" :background \"%s\" :inherit 'unspecified)"
                          my-live-face
                          (face-foreground my-live-face)
                          (face-background my-live-face))))
    (kill-new settings)
    (quit-window)
    (message "Copied settings to clipboard:\n\n%s" settings)))

(require 'subr-x)
(require 's)
(require 'url-parse)

(defun dnt--filter-qs (urlobj pred)
  (pcase (url-path-and-query urlobj)
    (`(,path . ,query)
     (let ((cleaned (cl-remove-if pred (url-parse-query-string query))))
       (setf (url-filename urlobj)
             (concat path (when cleaned
                            (concat "?" (url-build-query-string cleaned))))))))
  urlobj)

(defun dnt--clean-google-analytics (urlobj)
  "Return a URLOBJ with Google Analytics tracking removed."
    (url-recreate-url (dnt--filter-qs urlobj (lambda (kv) (s-starts-with? "utm_" (car kv))))))

(defun dnt--clean-amazon (urlobj)
  "Return a URLOBJ with Amazon tracking removed."
  (setf (url-filename urlobj) (car (s-split-up-to "ref=" (car (url-path-and-query urlobj)) 1)))
  (url-recreate-url urlobj))

(defun dnt--extract-url-from-query (urlobj param)
  "Return a URLOBJ from the PARAM query of a different URL."
  (cadr (assoc param (url-parse-query-string (cdr (url-path-and-query urlobj))))))

(defun dnt--clean (url)
  "Return a URL with one layer of tracking services removed."
  (let* ((urlobj (url-generic-parse-url url)))
    (cond
     ((s-contains? "utm_" url)
      (dnt--clean-google-analytics urlobj))

     ((s-contains? "amazon" (url-host urlobj))
      (dnt--clean-amazon urlobj))

     ((string= "out.reddit.com" (url-host urlobj))
      (dnt--extract-url-from-query urlobj "url"))

     (t url))))

(defun dnt (url)
  "Return a URL with tracking services removed."
  (let ((new (dnt--clean url)))
    (if (string= url new)
        url
      (dnt new))))

(defun dnt-at-point ()
  "Use dnt on the URL at point and add ouput to kill ring."
  (interactive)
  (when-let (url (ffap-url-at-point))
      (kill-new (dnt url))
    (message "Copied to kill ring: %s" (dnt url))))

(defun dnt-from-kill-ring ()
"Remove tracking strings from URL in kill ring and add output to kill ring."
(interactive)
(let ((new (dnt (current-kill 0))))
(if (string= (current-kill 0) new)
        (current-kill 0)
(kill-new new)
(message "Copied to kill ring: %s" new))))

(require 'parent-mode)
(defun parent-mode-display ()
  "Display this buffer's mode hierarchy."
  (interactive)
  (let ((ls (parent-mode-list major-mode)))
    (princ ls)))

(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes."
  (interactive)
  (let* ((totals ())
         (buffers (buffer-list()))
         (total-buffers (length buffers))
         (ht (make-hash-table :test 'equal)))
    (save-excursion
      (dolist (buffer buffers)
        (set-buffer buffer)
        (let
            ((mode-name (symbol-name major-mode)))
          (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals)))
             ht)
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                      total-buffers (length totals)))
      (dolist (item totals)
        (let
            ((key (car item))
             (count (cadr item)))
          (if (equal (substring key -5) "-mode")
              (setq key (substring key 0 -5)))
          (princ (format "%2d %20s %s\n" count key
                         (make-string count ?+))))))))

(defun dabbrev-completion-all-buffers ()
    (interactive)
  (setq current-prefix-arg '(16))
(call-interactively 'dabbrev-completion))

(evil-define-key '(emacs insert) text-mode-map
(kbd "C-n") 'dabbrev-completion-all-buffers)

(evil-define-key '(emacs insert) prog-mode-map
(kbd "C-n") 'dabbrev-completion-all-buffers)

(setq dabbrev-abbrev-skip-leading-regexp "~")

(require 'recentf)
(recentf-mode 1)

(setq recentf-exclude '(".*-autoloads\\.el\\'"
                        "[/\\]\\elpa/"
                        "bookmark"
                        ))

(define-key global-map "\M-[" 'transpose-frame)
(define-key global-map "\M-]" 'rotate-frame)

(require 'ibuffer)
 (global-set-key (kbd "C-x C-b") 'ibuffer)
    (autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups
      '(("default"
	       ("Dired" (mode . dired-mode))
	       ("Org" (derived-mode . org-mode))
               ("Text" (name . "^.*txt$"))
               ("Markdown" (derived-mode . markdown-mode))

	       ("Emacs Lisp" (mode . emacs-lisp-mode))
               ("Help" (or (derived-mode . help-mode)
                       (derived-mode . helpful-mode)
                       (derived-mode . elisp-refs-mode)
                       (derived-mode . apropos-mode)))
               ("Info" (derived-mode . Info-mode))
               ("Custom" (derived-mode . Custom-mode))
               ("Scratch" (name . "*scratch*"))
               ("Git" (derived-mode . magit-mode))
	       ("Other"
                  (or
                   (name . "^\\*")))
	       )))
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
      '((:eval (format-mode-line '("%e" evil-mode-line-tag mode-line-front-space (current-input-method (emojify string "🇬🇷")) " " (:eval (propertize
      "%Z"
      'help-echo 'mode-line-mule-info-help-echo
      'mouse-face 'mode-line-highlight
      'local-map mode-line-coding-system-map)) " " mode-line-buffer-identification " " (:eval
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

(evil-define-key 'emacs org-mode-map
(kbd "<down-mouse-1>") 'mouse-drag-region)

(evil-define-key 'insert org-mode-map
(kbd "<down-mouse-1>") 'mouse-drag-region)

(evil-define-key 'motion org-mode-map
(kbd "<down-mouse-1>") 'mouse-drag-region)

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
;; completely hide the leading stars
(setq org-bullets-invisible-leading-stars t)

(setq org-ellipsis "↪")
(set-face-attribute 'org-ellipsis nil :foreground "cyan3" :underline 'unspecified)

(setq org-todo-keywords
   '((sequence "TODO(t)" "⏳ CURRENTLY(c)" "⏲ SOMEDAY(s)" "✘ CANCELLED(x)" "✔ DONE(d)")))

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

(when (version<= "9.2" (org-version))
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("o" . "src org"))
(add-to-list 'org-structure-template-alist '("vim" . "src vim"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-tempo-keywords-alist '("t" . "title")))

(setq org-use-speed-commands
        (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))

(push '(org-goto . tree) org-show-context-detail)

(setq org-adapt-indentation nil)

(defun modi/org-in-any-block-p ()
  "Return non-nil if the point is in any Org block.

The Org block can be *any*: src, example, verse, etc., even any
Org Special block.

This function is heavily adapted from `org-between-regexps-p'."
  (save-match-data
    (let ((pos (point))
          (case-fold-search t)
          (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
          (limit-up (save-excursion (outline-previous-heading)))
          (limit-down (save-excursion (outline-next-heading)))
          beg end)
      (save-excursion
        ;; Point is on a block when on BLOCK-BEGIN-RE or if
        ;; BLOCK-BEGIN-RE can be found before it...
        (and (or (org-in-regexp block-begin-re)
                 (re-search-backward block-begin-re limit-up :noerror))
             (setq beg (match-beginning 0))
             ;; ... and BLOCK-END-RE after it...
             (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                         (match-string-no-properties 1)
                                         "\\( .*\\)*$")))
               (goto-char (match-end 0))
               (re-search-forward block-end-re limit-down :noerror))
             (> (setq end (match-end 0)) pos)
             ;; ... without another BLOCK-BEGIN-RE in-between.
             (goto-char (match-beginning 0))
             (not (re-search-backward block-begin-re (1+ beg) :noerror))
             ;; Return value.
             (cons beg end))))))

(defun modi/org-split-block ()
  "Sensibly split the current Org block at point.

If the point is anywhere on the line, but not at the beginning of the line 
(BOL), go to the end of the line, and then split the block.

Otherwise (if point is at BOL), split the block exactly at that point."
  (interactive)
  (if (modi/org-in-any-block-p)
      (save-match-data
        (save-restriction
          (widen)
          (let ((case-fold-search t)
                (at-bol (bolp))
                block-start
                block-end)
            (save-excursion
              (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
              (setq block-start (match-string-no-properties 0))
              (setq block-end (replace-regexp-in-string
                               "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
                               (match-string-no-properties 1))))
            ;; Go to the end of current line, if not at the BOL
            (unless at-bol
              (end-of-line 1))
            (insert (concat (if at-bol "" "\n")
                            block-end
                            "\n\n"
                            block-start
                            (if at-bol "\n" "")))
            ;; Go to the line before the inserted "#+begin_ .." line
            (beginning-of-line (if at-bol -1 0)))))
    (message "Point is not in an Org block")))

(require 'undo-tree)

(global-undo-tree-mode)

(define-key evil-emacs-state-map "\M-p" 'undo-tree-undo)
(define-key evil-emacs-state-map "\M-n" 'undo-tree-redo)

(require 'drag-stuff)

(add-hook 'text-mode-hook 'drag-stuff-mode)
(add-hook 'prog-mode-hook 'drag-stuff-mode)

(drag-stuff-define-keys)

(require 'espy)
(if (eq system-type 'gnu/linux)
    (setq espy-password-file "~/Λήψεις/σημαντικά αρχεία txt/passwords.org")
(setq espy-password-file "c:/temporarity folder/kalokairi 2019 folder/σημαντικά αρχεία txt/passwords.org"))
(setq espy-pass-prefix "password:")

(ivy-mode 1)
(counsel-mode 1)

(ivy-prescient-mode 1)

(prescient-persist-mode 1)

(setq prescient-sort-length-enable nil)

(setq ivy-sort-max-size 44000)

(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-M-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "\C-xb") 'counsel-switch-buffer)
(global-set-key (kbd "\C-cu") 'counsel-unicode-char)
(global-set-key (kbd "\C-cr") 'counsel-buffer-or-recentf)
(global-set-key (kbd "\C-h v") 'counsel-describe-variable)
(global-set-key (kbd "\C-h f") 'counsel-describe-function)

(with-eval-after-load 'swiper
(define-key swiper-map "\C-w" 'ivy-yank-symbol))

(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-partial-or-done)
(define-key ivy-minibuffer-map (kbd "TAB") #'ivy-alt-done)

(setq ivy-wrap t)

(setq ivy-initial-inputs-alist nil)

(setq ivy-extra-directories nil)

(setq swiper-goto-start-of-match t)

(setq swiper-stay-on-quit t)

(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(evil-set-initial-state 'ivy-occur-mode 'emacs)

(add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function))

(add-to-list 'ivy-switch-buffer-faces-alist '(emacs-lisp-mode . font-lock-keyword-face))
(add-to-list 'ivy-switch-buffer-faces-alist '(helpful-mode . font-lock-comment-face))
(add-to-list 'ivy-switch-buffer-faces-alist '(ivy-occur-mode . font-lock-comment-face))

(when (package-installed-p 'ivy)
(set-face-attribute 'ivy-org nil :inherit font-lock-function-name-face))

(when (package-installed-p 'ivy)
(set-face-attribute 'ivy-highlight-face nil :inherit font-lock-function-name-face))

(when (package-installed-p 'ivy)
(set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground "blue" :background "pale turquoise" :weight 'bold :inherit 'unspecified))

(define-key org-mode-map (kbd "\C-co") 'counsel-outline)
(setq counsel-org-headline-display-todo t)
(setq counsel-outline-face-style 'org)
(setq counsel-outline-path-separator "→")

(require 'ivy-rich)
(ivy-rich-mode 1)

(plist-put ivy-rich-display-transformers-list
             'try
    '(:columns
     ((ivy-rich-candidate (:width 30))
      (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
      (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
      (ivy-rich-package-install-summary (:face font-lock-doc-face)))))

(plist-put ivy-rich-display-transformers-list
             'package-reinstall
    '(:columns
     ((ivy-rich-candidate (:width 30))
      (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
      (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
      (ivy-rich-package-install-summary (:face font-lock-doc-face)))))

(plist-put ivy-rich-display-transformers-list
'counsel-switch-buffer
  '(:columns
   ((ivy-rich-candidate (:width 30))  ; return the candidate itself
    (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
    (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
   :predicate
   (lambda (cand) (get-buffer cand))))

(defun +ivy-rich-describe-variable-transformer (cand)
  "Previews the value of the variable in the minibuffer"
  (let* ((sym (intern cand))
         (val (and (boundp sym) (symbol-value sym)))
         (print-level 3))
    (replace-regexp-in-string
     "[\n\t\^[\^M\^@\^G]" " "
     (cond ((booleanp val)
            (propertize (format "%s" val) 'face
                        (if (null val)
                            'font-lock-comment-face
                          'font-lock-function-name-face)))
           ((symbolp val)
            (propertize (format "'%s" val)
                        'face 'font-lock-keyword-face))
           ((keymapp val)
            (propertize "<keymap>" 'face 'font-lock-constant-face))
           ((listp val)
            (prin1-to-string val))
           ((stringp val)
            (propertize (format "%S" val) 'face 'font-lock-string-face))
           ((numberp val)
            (propertize (format "%s" val) 'face 'font-lock-doc-face))
           ((format "%s" val)))
     t)))

        (plist-put ivy-rich-display-transformers-list
                   'counsel-describe-variable
                   '(:columns
                     ((counsel-describe-variable-transformer (:width 40)) ; the original transformer
                      (+ivy-rich-describe-variable-transformer (:width 10))
                      (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))))

(defun lmintmate/ivy-rich-file-last-modified-time (candidate)
  (if (file-remote-p candidate)
      "?"
    (format-time-string "%d/%m/%y %H:%M" (nth 5 (file-attributes candidate)))))

(plist-put ivy-rich-display-transformers-list
'counsel-recentf
    '(:columns
      ((lmintmate/ivy-rich-file-last-modified-time (:face font-lock-comment-face))
       (ivy-rich-candidate (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.8))))))))

(plist-put ivy-rich-display-transformers-list
'counsel-buffer-or-recentf
    '(:columns
      ((lmintmate/ivy-rich-file-last-modified-time (:face font-lock-comment-face))
       (counsel-buffer-or-recentf-transformer (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.8))))))))

(ivy-rich-set-display-transformer)

(evil-set-initial-state 'helpful-mode 'emacs)
(evil-set-initial-state 'elisp-refs-mode 'emacs)

(evil-define-key 'emacs special-mode-map
(kbd "\C-]") 'evil-jump-to-tag)

(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

(setq counsel-descbinds-function #'helpful-callable)

(global-set-key (kbd "C-h k") #'helpful-key)

(add-to-list 'ibuffer-help-buffer-modes 'helpful-mode)

(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

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

(when (package-installed-p 'trashed)
(require 'trashed)
(setq trashed-date-format "%a %d %b %Y %T")
(evil-set-initial-state 'trashed-mode 'emacs))

(setq show-paren-delay 0)
(show-paren-mode 1)
(setq show-paren-style 'mixed)

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

(setq ediff-split-window-function 'split-window-horizontally)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'dired-aux)
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
