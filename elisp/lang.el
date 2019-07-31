;;; lang.el --- Sum
;;; Commentary:
;;; Code:

(use-package company
  :straight t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers nil)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t))

(use-package yaml-mode :straight t)

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package tide
  :straight t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . 'tide-setup)
          (typescript-mode . 'tide-hl-identifier-mode)
          (before-save . 'tide-format-before-save)))

(use-package js-mode
  :straight nil
  :straight prettier-js
  :mode (("\\.js$" . 'js-mode))
  :hook ((js-mode . (lambda ()
                       (flycheck-mode))))
  :config
  (setq flycheck-javascript-eslint-executable "eslint")
  (prettier-js-mode))

(use-package add-node-modules-path
  :straight t
  :hook ((js-mode . 'add-node-modules-path)
          (rjsx-mode . 'add-node-modules-path))
          (prettier-js-mode . 'add-node-modules-path))

(provide 'lang)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; lang.el ends here
