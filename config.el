(setq inhibit-splash-screen t
inhibit-startup-message t
inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
(scroll-bar-mode -1))
(mouse-wheel-mode t)
(setq custom-safe-themes t)
(if (window-system)
(set-frame-font "Monospace 11"))
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq redisplay-dont-pause t
scroll-margin 1
scroll-step 1
scroll-conservatively 100
scroll-preserve-screen-position 1
jit-lock-defer-time 0.05
font-lock-support-mode 'jit-lock-mode)
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)
(global-visual-line-mode)
(add-hook 'after-init-hook 'global-whitespace-mode)
(setq whitespace-style (list 'space-mark 'tab-mark))
(global-hl-line-mode +1)
(global-linum-mode)

(setq make-backup-files nil)
(setq auto-save-default nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; C-x n n
;;   Narrow down to between point and mark (narrow-to-region).
;;   This can really freak you out, don't enable it unless you
;;   practice getting out of it.
;; C-x n w
;;   Widen to make the entire buffer accessible again (widen).
(put 'dired-find-alternate-file 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-always-indent 'complete)
(setq-default indicate-empty-lines t)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(delete-selection-mode t)
(setq show-paren-delay 0)
(show-paren-mode t)
(column-number-mode t)
(setq uniquify-buffer-name-style 'forward)
(global-auto-revert-mode t)
(windmove-default-keybindings)
(recentf-mode 1)
(setq recentf-max-menu-items 15
  recentf-max-saved-items 50
  recentf-auto-cleanup 'never)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(setq sentence-end-double-space nil)
(setq-default word-wrap t)
(setq truncate-partial-width-windows t)
(winner-mode 1)
(save-place-mode 1)
(setq large-file-warning-threshold 100000000)
(global-unset-key (kbd "C-z"))

(use-package inkpot-theme
:ensure t
:config
(load-theme 'inkpot t))

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-center-theme))

(setq-default mode-line-format
'("%e"
mode-line-front-space
;; mode-line-mule-info -- I'm always on utf-8
mode-line-client
mode-line-modified
;; mode-line-remote -- no need to indicate this specially
;; mode-line-frame-identification -- this is for text-mode emacs only
" "
mode-line-directory
mode-line-buffer-identification
" "
mode-line-position
;;(vc-mode vc-mode)  -- I use magit, not vc-mode
(flycheck-mode flycheck-mode-line)
" "
mode-line-modes
mode-line-misc-info
mode-line-end-spaces))

(use-package beacon
:ensure t
:config
(beacon-mode +1))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(use-package diminish
  :ensure t
  :demand t
  :diminish (git-gutter-mode . "gg")
  :diminish (visual-line-mode . "ω")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "RET") 'newline-without-break-of-line)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C--") 'text-scale-decrease)
  (define-key evil-normal-state-map (kbd "C-+") 'text-scale-increase)
  (define-key evil-normal-state-map (kbd "C-=") 'text-scale-set)
  (setq evil-shift-width 2)
  ;;(define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (evil-scroll-up nil)))
  ;;(define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive) (evil-scroll-down nil)))
  (setq evil-move-cursor-back nil))

(defun newline-without-break-of-line ()
"1. move to end of the line.
2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)

    (newline-and-indent)))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-key
    ","  (lambda () (interactive) (ansi-term (getenv "SHELL")))
    "m"  'neotree-toggle
    "n"  'neotree-project-dir
    "."  'switch-to-previous-buffer
    "/"  'evil-search-highlight-persist-remove-all
    "h"  'help-map
    "ps" 'helm-projectile-ag
    "pa" 'helm-projectile-find-file-in-known-projects
    "z" 'zoom-window-zoom
    "be" (lambda () (interactive) (bookmark-jump "emacs"))
    "w"  'ace-window))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-search-highlight-persist
  :ensure t
  :config
  (global-evil-search-highlight-persist t))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode t))

(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-global-mode))
  (setq projectile-mode-line
      '(:eval (format " Proj[%s]" (projectile-project-name))))

(use-package neotree
  :ensure t
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (add-hook 'neotree-mode-hook
    (lambda ()
      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
      (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
      (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
      (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
      (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
      (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)

      (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
      (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)

      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
        (neotree-dir project-dir)
        (neotree-find file-name))
    (message "Could not find git project root."))))

(use-package find-file-in-project :ensure t)
(use-package ag
  :ensure t
  :config
(setq ag-reuse-buffers 't)
(setq ag-reuse-window 't)
(setq ag-highlight-search t))
(use-package helm
  :ensure t
  :diminish helm-mode
  :config
(helm-mode 1)
(setq helm-buffers-fuzzy-matching t)
(setq helm-autoresize-mode t)
(setq helm-buffer-max-length 40)
(global-set-key (kbd "M-x") #'helm-M-x)
(define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
(define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))
(use-package helm-ag
  :ensure t)
(use-package helm-projectile
:bind (("C-S-P" . helm-projectile-switch-project)
       :map evil-normal-state-map
       ("C-p" . helm-projectile))
:after (helm projectile evil)
:commands (helm-projectile helm-projectile-switch-project)
:ensure t)
