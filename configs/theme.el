;;; theme.el --- summary
;;; commentary:
;;; code:

(use-package atom-one-dark-theme
  :ensure t
  :config
  (progn (load-theme 'atom-one-dark t)))

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

(use-package beacon
  :ensure t
  :config
  (beacon-mode +1))

;;; theme.el ends here
