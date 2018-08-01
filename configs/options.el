(if (window-system)
    (set-frame-font "Monospace 11"))

(if window-system
    (progn
      (setq frame-title-format '(buffer-file-name "%@%b%*"))
      (tooltip-mode -1)
      (mouse-wheel-mode t)
  (menu-bar-mode -1)))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(blink-cursor-mode -1)
(setq-default cursor-type '(bar . 2))
(global-hl-line-mode t)
(delete-selection-mode t)
(transient-mark-mode t)
(size-indication-mode t)
(show-paren-mode t)
(column-number-mode t)
(save-place-mode t)
(global-auto-revert-mode t)
;; (ido-mode t)
(global-display-line-numbers-mode)

(add-hook 'after-init-hook 'global-whitespace-mode)
(setq whitespace-style (list 'space-mark 'tab-mark 'newline-mark))

(setq debug-on-error t
      gc-cons-threshold 50000000
      large-file-warning-threshold 100000000
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      load-prefer-newer t
      sentence-end-double-space nil
      initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))
      inhibit-startup-message t
      inhibit-splash-screen t
      require-final-newline t
      next-line-add-newlines nil
      show-trailing-whitespace t
      xterm-mouse-mode t
      diff-switches "-u"
      cua-mode nil
      cua-auto-tabify-rectangles nil
      vc-follow-symlinks t
      gdb-many-windows t
      uniquify-buffer-name-style 'forward uniquify-separator "/")

(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq tab-always-indent 'complete)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
