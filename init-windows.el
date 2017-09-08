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
;; adds melpa repository
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; settings for multiple buffer management
(load-file "~/.emacs.d/buffer-defuns.el")

;; other config parameters
(setq auto-save-default nil)
(setq delete-by-moving-to-trash t)
(setq delete-selection-mode t)
(setq make-backup-files nil)
;; Toggling the menu bar with a keyboard shortcut
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)
;; toggling the scroll bar with a keyboard shortcut
(global-set-key [f10] 'toggle-scroll-bar)

;; visual line mode only for text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; associate .txt files with goto-address-mode, that highlights urls and makes them clickable
(add-hook 'find-file-hook
          (lambda ()
            (when (string= (file-name-extension buffer-file-name) "txt")
              (goto-address-mode 1))))

;; Adds shift + arrows for changing buffer, in addition to Ctrl+O
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; blue mood theme
(load-theme 'blue-mood t t)
(enable-theme 'blue-mood)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "DodgerBlue4")))))

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
 
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
