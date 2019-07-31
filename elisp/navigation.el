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

(use-package ag :straight t)

(use-package projectile
  :straight t
  :diminish projectile "P"
  :init
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode)
  :bind
  ("s-p" . projectile-find-file)
  ("s-g" . projectile-ag)
  ("s-q" . projectile-replace))

(use-package ivy
  :straight t
  :diminish ivy ""
  :config (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
    '((t . ivy--regex-fuzzy)))
  (ivy-set-actions
    'ivy-switch-buffer
    '(("j" switch-to-buffer-other-frame "other frame")
       ("k" kill-buffer "kill")
       ("r" ivy--rename-buffer-action "rename"))))

(use-package swiper
  :straight t
  :bind
  ("\C-s" . 'swiper))

(use-package counsel
  :straight t
  :after ivy
  :straight smex
  :straight flx
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file))

(use-package persp-projectile
  :straight t
  :init
  (persp-mode)
  :bind (:map projectile-mode-map
          ("s-n" . projectile-persp-switch-project)))

(use-package neotree :straight t)

(provide 'navigation)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; navigation.el ends here
