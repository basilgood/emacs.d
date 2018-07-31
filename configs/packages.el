(use-package atom-one-dark-theme
  :ensure t
  :config
  (progn (load-theme 'atom-one-dark t)))

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (sml/apply-theme 'dark))
