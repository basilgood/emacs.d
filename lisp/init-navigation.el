;;; init-navigation.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))
  (global-set-key (kbd "C-x j") 'dired-jump)

(when (maybe-require-package 'diff-hl)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(require-package 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-window 't)
(setq ag-reuse-buffers 't)

(require-package 'flx)
(require-package 'helm-flx)
(require-package 'helm)
(add-hook 'after-init-hook 'helm-mode)
(with-eval-after-load 'helm-mode
  (diminish 'helm-mode))
(add-hook 'after-init-hook 'helm-flx-mode)
(with-eval-after-load 'helm-flx-mode)
(setq helm-M-x-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
(setq helm-bookmark-show-location t)
(setq helm-buffer-max-length 30)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

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
  (setq-default projectile-mode-line-prefix " P")

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

  (maybe-require-package 'ibuffer-projectile))

(when(maybe-require-package 'persp-projectile)
  (add-hook 'after-init-hook 'persp-mode)
  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "s-p n") 'projectile-persp-switch-project)))

(when(maybe-require-package 'eyebrowse)
  (diminish eyebrowse-mode)
  (add-hook 'after-init-hook 'eyebrowse-mode)
  (with-eval-after-load 'eyebrowse
    (progn
      (define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
      (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
      (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
      (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
      (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
      (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
      (define-key eyebrowse-mode-map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
      (define-key eyebrowse-mode-map (kbd "M-7") 'eyebrowse-switch-to-window-config-7))))
(setq eyebrowse-mode-line-separator " "
  eyebrowse-new-workspace t)

(provide 'init-navigation)
;;; init-navigation.el ends here
