;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require-package 'gruvbox-theme)
;; (load-theme 'gruvbox-dark-hard t)
;; (require-package 'zerodark-theme)
;; (load-theme 'zerodark t)
(require-package 'atom-one-dark-theme)
(load-theme 'atom-one-dark t)
(atom-one-dark-with-color-variables
(custom-theme-set-faces
 'atom-one-dark
 `(whitespace-space                         ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-mono-3 :weight light))))
 `(whitespace-hspace                        ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-mono-3))))
 `(whitespace-tab                           ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-mono-3))))
 `(whitespace-newline                       ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-mono-3))))
 `(whitespace-trailing                      ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-red-1))))
 `(whitespace-line                          ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-red-1))))
 `(whitespace-space-before-tab              ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-mono-3))))
 `(whitespace-indentation                   ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-mono-3))))
 `(whitespace-empty                         ((t (:background ,nil :foreground ,nil))))
 `(whitespace-space-after-tab               ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-mono-3))))))

(require-package 'whitespace)
(add-hook 'after-init-hook 'global-whitespace-mode)
(with-eval-after-load 'global-whitespace-mode
  (diminish 'whitespace-mode))

(progn
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))
  (setq whitespace-display-mappings
        '(
          (space-mark 32 [183] [46])
          (tab-mark 9 [9655 9] [92 9])
          )))
(global-set-key (kbd "C-c w") 'whitespace-cleanup)

(defun dejavu-font-check (&optional frame)
  (when frame
    (select-frame frame))
  (condition-case nil
      (set-frame-font
       "SauceCodePro Nerd Font")
    (error
     (set-frame-font
      "DejaVuSansMono Nerd Font"))))
(dejavu-font-check)
(set-face-attribute 'default nil :height 120 :weight 'regular)
(add-hook 'after-make-frame-functions 'dejavu-font-check)

(set-face-background 'mode-line "#38322a")
(set-face-background 'mode-line-inactive "#1c1c1c")
(set-face-foreground 'mode-line-inactive "#5e5e5e")
(custom-set-faces
   '(mode-line ((t (:box (:line-width 2 :color "black"))))))

(provide 'init-themes)
;;; init-themes.el ends here
