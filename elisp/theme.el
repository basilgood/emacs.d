;;; theme.el --- Themes for graphical and terminal sessions
;;; Commentary:
;;; Code:

(defun single-font-size ()
  "Reset all faces to the height of the default face."
  (dolist (f (face-list))
    (when (not (equal 'default f))
      (set-face-attribute f nil :height 1.0))))

(set-face-attribute 'default nil
  :family "DejaVu Sans Mono"
  :height 110
  :weight 'normal
  :width 'normal
  :underline nil)

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :demand t
  :config
  (setf custom-safe-themes t)
  (color-theme-sanityinc-tomorrow-night)
  (global-hl-line-mode 1)
  (custom-set-faces
    '(cursor ((t :background "#eebb28")))))

(setq-default display-line-numbers 'directly
              display-line-numbers-width 3
              display-line-numbers-widen t)
(set-face-attribute 'line-number nil
                    :font "Iosevka Nerd Font-10"
                    :background "#282c34" :foreground "#5c6370")
(set-face-attribute 'line-number-current-line nil
                    :font "Iosevka Nerd Font-10"
                    :background "grey" :foreground "black")


(use-package spaceline
  :straight t
  :init
  (require 'spaceline-config)
  :config
  ;; segments
  (spaceline-spacemacs-theme)
  ;; (spaceline-emacs-theme)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-id-on)
  (spaceline-toggle-remote-host-on)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-line-column-on)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-projectile-root-on)
  (spaceline-toggle-window-number-on)
  ;; numbers
  (setq spaceline-window-numbers-unicode t)
  (setq spaceline-workspace-numbers-unicode t))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :diminish whitespace ""
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '('tabs tab-mark)))(provide 'theme)

(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show trailing whitespace between t and nil."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; theme.el ends here
