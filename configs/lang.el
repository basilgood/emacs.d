(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"

  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode)

  :bind
  (([(meta down)] . ruby-forward-sexp)
   ([(meta up)]   . ruby-backward-sexp)
    (("C-c C-e"    . ruby-send-region))))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.mkd\\'" "\\.markdown\\'"))

(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js$"
  :config
  (add-hook 'js2-mode-hook 'editorconfig-conf-mode))

(use-package eslintd-fix
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)
  (setq eslintd-fix-executable "~/.n/bin/eslint_d"))

(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json$")

(use-package yaml-mode
  :ensure t
  :mode "\\.e?ya?ml$")

(use-package ledger-mode
  :ensure t
  :defer t)

(use-package jinja2-mode
  :ensure t
  :defer t
  :mode "\\.j2$")

(use-package twig-mode
  :ensure t
  :defer t
  :mode "\\.twig$")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee$\\'")

;;; lang.el ends here
