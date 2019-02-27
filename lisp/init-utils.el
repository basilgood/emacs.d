;;; init-utils.el --- Utils packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Huge files
(require-package 'vlf)
(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))

(setq large-file-warning-threshold 100000000)
(add-hook 'find-file-hook
          (defun my-find-file-care-about-long-lines ()
            (save-excursion
              (goto-char (point-min))
              (when (and (not (eq major-mode 'image-mode))
                         (search-forward-regexp ".\\{2000\\}" 50000 t)
                         (y-or-n-p "Very long lines detected - enable longlines-mode? "))
                (require 'longlines)
                (longlines-mode +1)))))

(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(with-eval-after-load 'page-break-lines
  (push 'browse-kill-ring-mode page-break-lines-modes))

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-;") 'avy-goto-char))

(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)
(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c u") 'md/duplicate-up)

;; Cut/copy the current line if no region is active
(require-package 'whole-line-or-region)
(add-hook 'after-init-hook 'whole-line-or-region-mode)
(with-eval-after-load 'whole-line-or-region
  (diminish 'whole-line-or-region-local-mode))

;; Delete word under cursor
(defun my-kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun my-kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (my-kill-thing-at-point 'word))

(global-set-key (kbd "s-k") 'my-kill-word-at-point)

;; Multiple cursors
(require-package 'multiple-cursors)
(global-set-key (kbd "s-n") 'mc/mark-next-like-this)
(global-set-key (kbd "s-m") 'mc/mark-previous-like-this)

(require-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(with-eval-after-load 'which-key
  (diminish 'which-key-mode))

(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)

(require-package 'multi-term)
(setq multi-term-program "/run/current-system/sw/bin/zsh")
(global-set-key (kbd "C-x C-m") 'multi-term)
(global-set-key (kbd "C-x m") 'multi-term-next)

(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map  (kbd "C-y") 'term-paste)

(require-package 'smartparens)
(require 'smartparens-config)
(add-hook 'after-init-hook 'smartparens-global-mode)
(with-eval-after-load 'smartparens
  (diminish 'smartparens-mode))

(require-package 'dot-mode)
(add-hook 'after-init-hook 'global-dot-mode)
(with-eval-after-load 'dot-mode
  (diminish 'dot-mode))

(provide 'init-utils)
;;; init-utils.el ends here
