;;; package --- init file
;;; Commentary:
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package use-package-ensure-system-package
  :straight t)
(setq use-package-always-defer t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; for faster emacs start-up; gets re-set later
(setq gc-cons-threshold (* 50 1000 1000))

;; set load path
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(setq ad-redefinition-action 'accept)

(require 'base)
(require 'theme)
(require 'utils)
(require 'navigation)
(require 'flycheck)
(require 'lang)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
