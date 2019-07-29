;;; base.el --- Sum
;;; Commentary:
;;; Code:

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(blink-cursor-mode -1)

(setq-default
  bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
  buffers-menu-max-size 30
  case-fold-search t
  column-number-mode t
  delete-selection-mode t
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain
  indent-tabs-mode nil
  tab-width 2
  make-backup-files nil
  mouse-yank-at-point t
  save-interprogram-paste-before-kill t
  scroll-preserve-screen-position 'always
  scroll-conservatively 100
  set-mark-command-repeat-pop t
  tooltip-delay 1.5
  truncate-lines t
  truncate-partial-width-windows nil
  x-wait-for-event-timeout nil)

(global-auto-revert-mode t)

(add-hook 'after-init-hook 'transient-mark-mode)

(set-frame-parameter (selected-frame) 'alpha '(100 100))

(global-hl-line-mode t)

(when (fboundp 'display-line-numbers-mode)
  (setq display-line-numbers-width-start t)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(global-unset-key (kbd "C-z"))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'after-init-hook 'show-paren-mode)

(cua-selection-mode t)

(windmove-default-keybindings)

(electric-pair-mode t)

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(use-package diminish :straight t)

(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (setq-default save-place t))

(save-place-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

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
