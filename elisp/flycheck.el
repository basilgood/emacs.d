;;; flycheck.el --- sum
;;; Commentary:
;;; Code:

(use-package flycheck
  :straight t
  :diminish  flycheck ""
  :init (global-flycheck-mode))

(provide 'flycheck)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; flycheck.el ends here
