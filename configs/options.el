;; (setq-default gc-cons-threshold 10000000)

;; (set-face-attribute 'default nil :font "Hack-11")
;; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (setq-default inhibit-startup-message    t
;;               make-backup-files          nil
;;               vc-follow-symlinks         t
;;               frame-title-format         "%@%b%*"
;;               gdb-many-windows           t
;;               diff-switches              "-u"
;;               scroll-step                10
;;               cua-mode                   nil
;;               cua-auto-tabify-rectangles nil)
;; (save-place-mode t)
;; (setq show-paren-delay 0)
;; (show-paren-mode 1)
;; (line-number-mode 1)
;; (column-number-mode 1)
;; (size-indication-mode 1)
;; (transient-mark-mode 1)
;; (delete-selection-mode 1)
;; (set-default 'indent-tabs-mode nil)
;; (setq-default tab-width 2)
;; (setq sentence-end-double-space nil)
;; (setq require-final-newline t)
;; (fset 'yes-or-no-p 'y-or-n-p)
;; (xterm-mouse-mode t)
;; (global-linum-mode t)
;; (add-hook 'after-init-hook 'global-whitespace-mode)
;; (setq whitespace-style (list 'space-mark 'tab-mark 'newline-mark))

(mapc (lambda (mode) (funcall mode 1))
      '(auto-compression-mode
        global-auto-revert-mode
        global-font-lock-mode
        column-number-mode
        line-number-mode
        size-indication-mode
        ido-mode
        show-paren-mode
        subword-mode
        (xterm-mouse-mode t)
        (fset 'yes-or-no-p 'y-or-n-p)
        transient-mark-mode))

;; Activate modes

(mapc (lambda (mode) (funcall mode -1))
      '(menu-bar-mode
        scroll-bar-mode
        tool-bar-mode
        tooltip-mode))

;; Default variables

(setq user-full-name "Vasile Luta"
      debug-on-error t
      gc-cons-threshold 100000000
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      load-prefer-newer t
      sentence-end-double-space nil
      frame-title-format "%b (%m) - %F"
      initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))
      inhibit-startup-message t
      inhibit-splash-screen t
      case-fold-search t
      require-final-newline t
      next-line-add-newlines nil
      select-enable-clipboard t
      show-trailing-whitespace t
      uniquify-buffer-name-style 'forward uniquify-separator "/")

;; Indentation

(setq-default indent-tabs-mode nil
              tab-width 2)

;; Locale

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

