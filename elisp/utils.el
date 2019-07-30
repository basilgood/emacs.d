;;; utils.el --- summary-
;;; Commentary:
;;; Code:

(use-package blimp
  :straight t
  :init
  (add-hook 'image-mode-hook 'blimp-mode))

(use-package browse-at-remote
  :straight t
  :bind
  ("C-c b r" . 'browse-at-remote))

(use-package async
  :straight t
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package editorconfig
  :straight t
  :diminish editorconfig-mode ""
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

(use-package multiple-cursors
  :straight t
  :bind (("M-." . mc/mark-next-like-this)
          ("M-," . mc/unmark-next-like-this)
          ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package rainbow-delimiters
  :straight t)

(use-package evil-leader
  :straight t
  :demand t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "\\")
  (evil-leader/set-key
    "," 'other-window
    "." 'mode-line-other-buffer
    "b" 'counsel-switch-buffer
    "f" 'counsel-find-file
    "k" 'kill-this-buffer
    "\\" 'save-buffer
    "c" 'comment-line
    "x" 'evil-window-delete
    "n" 'neotree-toggle
    "e" 'eval-last-sexp
    "a" 'align-regexp
    ))

(use-package evil
  :straight t
  :init
  (evil-mode)
  :config
  (evil-define-key
    '(normal replace operator motion emacs)
    'global
    (kbd ";;") 'save-buffer)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :diminish undo-tree ""
  :config
  (setq undo-tree-history-directory-alist
    `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package evil-commentary
  :straight t
  :diminish evil-commentary ""
  :init
  (evil-commentary-mode))

(use-package evil-visualstar
  :straight t
  :init
  (global-evil-visualstar-mode))

(use-package evil-matchit
  :straight t
  :init
  (global-evil-matchit-mode))

(use-package evil-surround
  :straight t
  :init
  (global-evil-surround-mode))

(use-package volatile-highlights
  :straight t
  :diminish volatile-highlights-mode
  :init
  (volatile-highlights-mode t))

(use-package git-timemachine
  :straight t
  :init
  (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))

(use-package magit
  :straight t
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch))

(use-package diff-hl
  :straight t
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :straight t
  :diminish which-key ""
  :init
  (which-key-mode))

(provide 'utils)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; utils.el ends here
