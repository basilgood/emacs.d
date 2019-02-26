;;; init-navigation.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(when (maybe-require-package 'diff-hl)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(require-package 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-window 't)
(setq ag-reuse-buffers 't)

(require-package 'helm)
(add-hook 'after-init-hook 'helm-mode)
(with-eval-after-load 'helm-mode
  (diminish 'helm-mode))
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq
 helm-quick-update t
 helm-idle-delay 0.01
 helm-input-idle-delay 0.01
 helm-ff-search-library-in-sexp t
 helm-split-window-default-side 'other
 helm-split-window-in-side-p t
 helm-candidate-number-limit 100
 helm-M-x-fuzzy-match t
 helm-buffers-fuzzy-matching t
 helm-recentf-fuzzy-match t
 helm-semantic-fuzzy-match t
 helm-imenu-fuzzy-match t
 helm-split-window-in-side-p nil
 helm-move-to-line-cycle-in-source nil
 helm-ff-search-library-in-sexp t
 helm-scroll-amount 8 
 helm-echo-input-in-header-line t
 helm-M-x-fuzzy-match t
 helm-completion-in-region-fuzzy-match t)

(require 'helm-config)
(with-eval-after-load 'helm
  (helm-autoresize-mode))
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.381)))

(require-package 'helm-ag)
(global-set-key (kbd "M-p") 'helm-ag-project-root)
(setq helm-ag-insert-at-point 'symbol
      helm-ag-command-option "--path-to-ignore ~/.agignore")

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile))

(when(maybe-require-package 'persp-projectile)
  (add-hook 'after-init-hook 'persp-mode)
  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "s-p n") 'projectile-persp-switch-project)))
  

(provide 'init-navigation)
;;; init-navigation.el ends here
