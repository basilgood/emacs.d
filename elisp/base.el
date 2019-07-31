;;; base.el --- Sum
;;; Commentary:
;;; Code:

(if window-system
  (progn
    (toggle-scroll-bar 0)
    (tool-bar-mode 0)
    (menu-bar-mode 0)))

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq frame-title-format nil)
(setq ring-bell-function 'ignore)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq track-eol t)			; Keep cursor at end of lines.
(setq line-move-visual nil)		; To be required by track-eol
(setq-default kill-whole-line t)	; Kill line including '\n'
(setq-default indent-tabs-mode nil)   ; use space
(setq-default scroll-preserve-screen-position 'always)
(setq-default scroll-step 1)
(setq-default
  buffers-menu-max-size 30
  case-fold-search t
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain
  tab-width 2
  make-backup-files nil
  mouse-yank-at-point t
  save-interprogram-paste-before-kill t
  scroll-preserve-screen-position 'always
  scroll-conservatively 100
  set-mark-command-repeat-pop t
  tooltip-delay 1.5
  make-backup-files nil
  auto-save-default nil
  truncate-lines t
  truncate-partial-width-windows nil
  x-wait-for-event-timeout nil)

(blink-cursor-mode -1)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package frame
  :bind
  ("C-z" . nil))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(add-hook 'term-mode-hook
  (lambda ()
    (setq line-spacing 0)))

(delete-selection-mode t)
(column-number-mode t)
(global-auto-revert-mode t)

(add-hook 'after-init-hook 'transient-mark-mode)

(set-frame-parameter (selected-frame) 'alpha '(100 100))

(global-hl-line-mode t)

(global-unset-key (kbd "C-z"))

(add-hook 'after-init-hook 'show-paren-mode)

(cua-selection-mode t)

(windmove-default-keybindings)

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(use-package diminish :straight t)

(use-package recentf
  :straight nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 20000000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     "cache"
                     "recentf"
                     "COMMIT_EDITMSG\\'"))
  :preface
  (defun ladicle/recentf-save-list-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-save-list))
        (recentf-save-list)))
    (message ""))
  (defun ladicle/recentf-cleanup-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if shutup-p
          (shut-up (recentf-cleanup))
        (recentf-cleanup)))
    (message ""))
  :hook
  (focus-out-hook . (ladicle/recentf-save-list-silence ladicle/recentf-cleanup-silence)))

(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (setq-default save-place t)
  (save-place-mode))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package hungry-delete
  :straight t
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(use-package smartparens
  :straight t
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

(global-set-key (kbd "M-/") 'hippie-expand)
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

(provide 'base)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; base.el ends here
