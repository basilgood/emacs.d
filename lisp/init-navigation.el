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

(when (maybe-require-package 'helm)
  (require-package 'flx)
  (require-package 'smex)
  (require-package 'dash)
  (require-package 'helm-flx)
  (require-package 'helm-swoop)
  (require-package 'helm-fuzzier)
  (require-package 'helm-smex)
  (add-hook 'after-init-hook 'helm-mode 'helm-flx-mode 'helm-fuzzier-mode)
  (with-eval-after-load 'helm
    (diminish 'helm-mode)
    (progn
      (global-set-key (kbd "C-x C-f") #'helm-find-files)
      (global-set-key (kbd "C-x b") 'helm-buffers-list)
      (global-set-key (kbd "M-y") 'helm-show-kill-ring)
      (global-set-key [remap execute-extended-command] #'helm-smex)
      (global-set-key (kbd "M-X") #'helm-smex-major-mode-commands))))
(setq helm-always-two-windows nil)
(setq helm-display-buffer-default-height 15)
(setq helm-default-display-buffer-functions '(display-buffer-in-side-window))

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
