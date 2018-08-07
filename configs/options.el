;;; package --- summary
;;; commentary:
;;; code:

(setq inhibit-startup-message t)

(if (window-system)
    (set-frame-font "Inconsolata 12"))

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

(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 100
  scroll-preserve-screen-position 1
  jit-lock-defer-time 0.05
  font-lock-support-mode 'jit-lock-mode)
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)

(setq large-file-warning-threshold 100000000)

(setq-default indent-tabs-mode nil
  tab-width 2)

(delete-selection-mode t)

(global-auto-revert-mode t)

(windmove-default-keybindings)

(global-display-line-numbers-mode)

(save-place-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 15
  recentf-max-saved-items 50
  recentf-auto-cleanup 'never)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(bind-key "M-D" 'delete-pair)
(setq show-paren-delay 0)
(show-paren-mode t)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :underline 't)

(add-hook 'after-init-hook 'global-whitespace-mode)
(setq whitespace-style (list 'space-mark 'tab-mark 'newline-mark))

(setq sentence-end-double-space nil)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq truncate-partial-width-windows t)

(global-git-gutter-mode +1)
(custom-set-variables
  '(git-gutter:handled-backends '(git hg bzr svn)))

(setq-default word-wrap t)
