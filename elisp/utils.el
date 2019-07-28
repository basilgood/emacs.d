;;; utils.el
;;; Commentary:
;;; Code:

(use-package blimp
  :straight t
  :init
  (add-hook 'image-mode-hook 'blimp-mode))

(use-package browse-at-remote
  :straight t
  :bind
  (("C-c b r" . browse-at-remote)))

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

(use-package evil-leader
  :straight t
  :demand t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "\\")
  (evil-leader/set-key
    ","  'other-window
    "."  'mode-line-other-buffer
    "b"  'helm-buffers-list
    "x"  'helm-smex
    "p"  'helm-find-files
    "k"  'kill-this-buffer
    "\\"  'save-buffer
    "c" 'comment-line
    "h" 'split-and-follow-horizontally
    "v" 'split-and-follow-vertically
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
  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package volatile-highlights
  :straight t
  :diminish volatile-highlights-mode
  :init
  (volatile-highlights-mode t))

(use-package git-timemachine
  :straight t
  :config
  (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))



(use-package magit
  :straight t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch))

(provide 'utils)
;;; utils.el ends here
