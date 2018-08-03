(use-package diminish
  :ensure t
  :demand t
  :diminish (visual-line-mode . "ω")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function
  :diminish subword-mode)

(use-package bind-key :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package flyspell
  :diminish (flyspell-mode . "φ")
  :bind* (("M-m ] s" . flyspell-goto-next-error)))

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

(use-package uniquify
:config
(setq uniquify-buffer-name-style 'forward)
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
