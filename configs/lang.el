;;; lang.el --- summary
;;; commentary:
;;; code:

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

;; (use-package eslint-fix
;;   :ensure t
;;   :config
;;   (eval-after-load 'js2-mode
;; 	   '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))
;; ;;  (setq eslintd-fix-executable "~/.n/bin/eslint_d"))

(use-package mmm-mode
  :ensure t
  :commands mmm-mode
  :config
  (setq
   mmm-global-mode 'buffers-with-submode-classes
   mmm-submode-decoration-level 0)

  (use-package mmm-auto))

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

;; (use-package js2-mode
;;   :ensure t
;;   :init
;;   (progn
;;     (add-to-list 'auto-mode-alist '("\\.js?\\'" .
;; js2-jsx-mode))))

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

(use-package vimrc-mode
  :ensure t)


;;; lang.el ends here
