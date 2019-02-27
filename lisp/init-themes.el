;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'atom-one-dark t)

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
(set-face-attribute 'default nil :height 100 :weight 'regular)
(add-hook 'after-make-frame-functions 'dejavu-font-check)

(set-face-background 'mode-line "#38322a")
(set-face-background 'mode-line-inactive "#1c1c1c")
(set-face-foreground 'mode-line-inactive "#5e5e5e")
(custom-set-faces
   '(mode-line ((t (:box (:line-width 2 :color "black"))))))

(provide 'init-themes)
;;; init-themes.el ends here
