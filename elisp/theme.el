;;; base-theme.el --- Themes for graphical and terminal sessions
;;
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
  (global-whitespace-mode t)
  :diminish whitespace-mode WS
  :config
  (progn
    (setq whitespace-style (quote (spaces tabs  space-mark tab-mark)))
    (setq whitespace-display-mappings
      '(
         (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
         ;; (tab-mark 9 [9655 9] [92 9]) ; tab
         (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])
         ))))

(provide 'theme)

;;; theme.el ends here
