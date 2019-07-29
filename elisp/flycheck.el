;;; flycheck.el --- sum
;;; Commentary:
;;; Code:

(use-package flycheck
  :straight t
  :diminish  flycheck ""
  ;; :commands flycheck-mode
  :init (global-flycheck-mode))
  ;;:config
  ;; (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  ;; :straight flycheck-color-mode-line
  ;; :demand
  ;; (flycheck-color-mode-line-mode))

  ;; (when (maybe-require-package 'flycheck-color-mode-line)
  ;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; (setq-default flycheck-disabled-checkers '(javascript-jshint))
;; (setq flycheck-eslintrc "./.eslintrc.json")
;; (setq flycheck-checkers '(javascript-eslint))

(provide 'flycheck)
;;; flycheck.el ends here
