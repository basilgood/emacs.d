(setq inhibit-startup-message t)

(if (window-system)
    (set-frame-font "Monospace 11"))

(if window-system
    (progn
      (setq frame-title-format '(buffer-file-name "%@%b%*"))
      (tooltip-mode -1)
      (mouse-wheel-mode t)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))

(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 2))
(setq ring-bell-function 'ignore)

(setq make-backup-files nil)
(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 100)

(setq large-file-warning-threshold 100000000)

(setq-default indent-tabs-mode nil
  tab-width 2)

(delete-selection-mode t)

(global-auto-revert-mode t)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                          try-expand-dabbrev-all-buffers
                                          try-expand-dabbrev-from-kill
                                          try-complete-file-name-partially
                                          try-complete-file-name
                                          try-expand-all-abbrevs
                                          try-expand-list
                                          try-expand-line
                                          try-complete-lisp-symbol-partially
                                          try-complete-lisp-symbol))

(windmove-default-keybindings)

(global-display-line-numbers-mode)

(save-place-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 15
  recentf-max-saved-items 50
  recentf-auto-cleanup 'never)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(show-paren-mode t)

(add-hook 'after-init-hook 'global-whitespace-mode)
(setq whitespace-style (list 'space-mark 'tab-mark 'newline-mark))

(setq sentence-end-double-space nil)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
