;;; theme.el --- Themes for graphical and terminal sessions
;;; Commentary:
;;; Code:

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :demand t
  :config
  (setf custom-safe-themes t)
  (color-theme-sanityinc-tomorrow-night)
  (global-hl-line-mode 1)
  (custom-set-faces
   '(cursor ((t :background "#eebb28")))))

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
  (setq whitespace-style '('spaces tabs space-mark tab-mark)))(provide 'theme)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; theme.el ends here
