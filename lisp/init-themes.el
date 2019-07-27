;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'atom-one-dark t)
(require-package 'zenburn-theme)
(load-theme 'zenburn t)
(with-eval-after-load "zenburn-theme"
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg)))))))

(require-package 'whitespace)
(add-hook 'after-init-hook 'global-whitespace-mode)
(diminish 'whitespace-mode)
(progn
  (setq whitespace-style (quote (spaces tabs  space-mark tab-mark)))
  (setq whitespace-display-mappings
    '(
       (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
       ;; (tab-mark 9 [9655 9] [92 9]) ; tab
       (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])
       )))

(set-frame-font "DejaVuSansMono Nerd Font 11")

(set-face-background 'mode-line "#38322a")
(set-face-background 'mode-line-inactive "#1c1c1c")
(set-face-foreground 'mode-line-inactive "#5e5e5e")
(custom-set-faces
   '(mode-line ((t (:box (:line-width 2 :color "black"))))))

(provide 'init-themes)
;;; init-themes.el ends here
