;;; init-lang.el --- Lang support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'editorconfig)
(add-hook 'after-init-hook 'editorconfig-mode)
  (with-eval-after-load 'editorconfig
    (diminish 'editorconfig-mode))

(when (maybe-require-package 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\.html\\'" . markdown-mode))
  (with-eval-after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))

(when (maybe-require-package 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\.erb\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode))

(when (maybe-require-package 'nix-mode)
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-hook 'nix-mode-common-hook
    (function (lambda ()
                (add-hook 'before-save-hook
                  'nix-mode-format)))))

(maybe-require-package 'json-mode)
(maybe-require-package 'coffee-mode)
(with-eval-after-load 'coffee-mode
  (setq-default coffee-js-mode js2-mode))
(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))
(maybe-require-package 'js2-mode)
(setq-default js2-bounce-indent-p nil)
(with-eval-after-load 'js2-mode
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil))
(add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))


(when (maybe-require-package 'add-node-modules-path)
  (with-eval-after-load 'typescript-mode
    (add-hook 'typescript-mode-hook 'add-node-modules-path))
  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook 'add-node-modules-path)))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
  '(lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(provide 'init-lang)
;;; init-lang.el ends here
