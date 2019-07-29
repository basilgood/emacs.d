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
  :bind (("M-x" . helm-smex)
          ("C-x C-f" . helm-find-files)
          ("C-x f" . helm-recentf)
          ("M-y" . helm-show-kill-ring)
          ("C-x b" . helm-buffers-list)))

(use-package helm-files
  :straight nil
  :bind (:map helm-find-files-map
          ("<left>" . helm-find-files-up-one-level)
          ("<right>" . helm-execute-persistent-action)))
(setq helm-always-two-windows nil
  helm-display-buffer-default-height 20
  helm-default-display-buffer-functions '(display-buffer-in-side-window))

(use-package neotree :straight t)

(provide 'navigation)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; navigation.el ends here
