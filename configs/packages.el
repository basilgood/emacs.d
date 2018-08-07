;;; package --- all packages
;;; commentary:
;;; code:

(use-package cl
  :ensure t)

(use-package diminish
  :ensure t
  :demand t
  :diminish (git-gutter-mode . "gg")
  :diminish (visual-line-mode . "ω")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function)

(use-package bind-key :ensure t)

(use-package subword
  :diminish subword-mode
  :init
  (global-subword-mode))

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
  :diminish (company-mode . "CO")
  :ensure t
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (progn)
  (setq company-idle-delay 0.5
    company-show-numbers nil
    company-tooltip-limit 10
    company-minimum-prefix-length 2
    company-tooltip-align-annotations t
    company-tooltip-flip-when-above t))

(use-package flex-autopair
  :ensure t
  :config
  (flex-autopair-mode))

(use-package iedit
  :ensure t)

(use-package vlf
  :ensure t)

(use-package smart-tabs-mode
  :ensure t
  :config
  (smart-tabs-insinuate 'c 'javascript))

;;; packages.el ends here
