;;; package --- all packages
;;; commentary:
;;; code:
(use-package cl
  :ensure t)

(use-package diminish
  :ensure t
  :demand t
  :diminish (visual-line-mode . "ω")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function)

(use-package bind-key :ensure t)

(use-package editorconfig
  :diminish (editorconfig-mode ."EC")
  :ensure t
  :config
  (editorconfig-mode t))

(use-package subword
  :diminish subword-mode
  :init
  (global-subword-mode)
)

(use-package flyspell
  :diminish (flyspell-mode . "φ")
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package restart-emacs
  :ensure t
  :bind* (("C-x M-c" . restart-emacs)))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

(use-package gitignore-mode
  :ensure t
  :config
  (add-hook 'gitignore-mode-hook (lambda ()
                                   (setq require-final-newline t))))

(use-package which-key
  :ensure t
  :config
    (which-key-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package uniquify
:config
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*"))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode)
  (diminish 'volatile-highlights-mode))

(use-package multiple-cursors
  :ensure t
  :bind (
          ("M-3" . mc/mark-next-like-this)
          ("M-4" . mc/mark-previous-like-this)
          :map ctl-x-map
          ("\C-m" . mc/mark-all-dwim)
          ("<return>" . mule-keymap)
          ))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :demand t
  :bind (("C-c h n" . git-gutter:next-hunk)
         ("C-c h p" . git-gutter:previous-hunk))
  :config
  (progn
    (global-git-gutter-mode t)
    (define-fringe-bitmap 'git-gutter-fr:added
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center)))

(use-package flex-autopair
  :ensure t
  :config
  (flex-autopair-mode))

;;; packages.el ends here
