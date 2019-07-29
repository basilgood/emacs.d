;;; lang.el --- Sum
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :straight t)
(use-package 'nix-mode)
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-hook 'nix-mode-common-hook
    (function (lambda ()
                (add-hook 'before-save-hook
                  'nix-mode-format)))))
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; lang.el ends here
