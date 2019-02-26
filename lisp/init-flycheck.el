;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(setq-default flycheck-disabled-checkers '(javascript-jshint))
;; (setq flycheck-eslintrc "./.eslintrc.json")
(setq flycheck-checkers '(javascript-eslint))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
