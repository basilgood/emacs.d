;;; navigation.el --- sum
;;; Commentary:
;;; Code:

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t)
  (require 'dired-x))

(use-package helm
  :straight t
  :demand t
  :straight flx
  :straight dash
  :straight helm-flx
  :straight helm-swoop
  :straight helm-fuzzier
  :straight helm-smex
  :hook (helm-mode)
  :hook (helm-flx-mode)
  :hook (helm-fuzzier-mode)
  :diminish helm-mode ""
  :bind ("M-x" . helm-smex)
          ("C-x C-f" . helm-find-files)
          ("C-x f" . helm-recentf)
          ("M-y" . helm-show-kill-ring)
          ("C-x b" . helm-buffers-list))

(use-package helm-files
  :straight nil
  :bind (:map helm-find-files-map
          ("<left>" . helm-find-files-up-one-level)
          ("<right>" . helm-execute-persistent-action)))
(setq helm-always-two-windows nil
  helm-display-buffer-default-height 20
  helm-default-display-buffer-functions '(display-buffer-in-side-window))

(use-package projectile
  :straight t
  :init
  (setq projectile-completion-system 'helm)
  (projectile-global-mode)
  :bind
  ("s-p" . projectile-find-file)
  ("s-g" . projectile-ag)
  ("s-q" . projectile-replace))

(use-package persp-projectile
  :straight t
  :init
  (persp-mode)
  :bind (:map projectile-mode-map
          ("s-n" . projectile-persp-switch-project)))

(use-package neotree :straight t)

(use-package eyebrowse
  :straight t
  :init
  (eyebrowse-mode)
  :config
  (progn
    (define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
    (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
    (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
    (define-key eyebrowse-mode-map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
    (define-key eyebrowse-mode-map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)))
(setq eyebrowse-mode-line-separator " "
  eyebrowse-new-workspace t)

(provide 'navigation)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; navigation.el ends here
