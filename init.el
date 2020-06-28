;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:

(setq gc-cons-threshold 402653184
  gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
    gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
  '("org" . "https://orgmode.org/elpa/") t)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Disable GUI stuff
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(fset #'display-startup-echo-area-message #'ignore)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(winner-mode t)

;;; Set the font
(set-face-attribute 'default nil :family "DejaVuSans Mono" :height 110)

;;; Disable lock files
(setq create-lockfiles nil)

;;; Disable backup files
(setq make-backup-files nil)

;;; Move auto-save files to saner location
(let ((auto-save-dir (file-name-as-directory (expand-file-name "autosave" user-emacs-directory))))
  (setq auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-dir))
  (setq auto-save-file-name-transforms (list (list ".*" (replace-quote auto-save-dir) t))))

;;; Use UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; Fix scrolling
(setq mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 'always)
(setq auto-window-vscroll nil)

;;; Use spaces
(setq indent-tabs-mode nil)
(setq tab-width 2)

;;; misc
(setq require-final-newline t
  delete-by-moving-to-trash t
  delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Do not disable commands
(setq disabled-command-function nil)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

(use-package diminish
  :ensure t
  :defer 2)

(use-package hydra
  :ensure t)

(use-package use-package-hydra
  :ensure t)

(use-package savehist
  :init
  (setq savehist-additional-variables
    '(search-ring regexp-search-ring)
    savehist-file (expand-file-name "savehist" user-emacs-directory))
  :config
  (savehist-mode 1))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-mode 1))

(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
    recentf-max-saved-items 500
    recentf-max-menu-items 15
    recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

(use-package windmove
  :init
  (windmove-default-keybindings))

(use-package hl-line
  :ensure nil
  :hook
  (after-init . global-hl-line-mode))

;;; show-paren-mode
(use-package paren
  :ensure nil
  :config
  (show-paren-mode)
  :init
  (setq show-paren-delay 0))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (undo-tree-mode))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-isearch-query-replace)
          ("C-M-%" . anzu-isearch-query-replace-regexp))
  :config
  (global-anzu-mode))

;;; Which key
(use-package which-key
  :ensure t
  :defer 2
  :diminish
  :config
  (which-key-mode))

;;; Navigation
(use-package async
  :ensure t)

(server-start)

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq
    dired-recursive-deletes 'always
    dired-recursive-copies 'always
    dired-dwim-target t
    dired-listing-switches "-alhv --group-directories-first"
    dired-no-confirm '(copy))
  (define-key dired-mode-map "-" 'dired-up-directory)
  (define-key dired-mode-map (kbd "<left>") 'dired-up-directory)
  (define-key dired-mode-map (kbd "<right>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
  (require 'dired-x)
  (dired-async-mode))

;;; Expand region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package projectile
  :ensure t
  :diminish
  :init
  (setq projectile-require-project-root nil
    projectile-enable-caching t
    projectile-completion-system 'ivy)
  :config
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :config
  (add-hook 'after-init-hook 'counsel-projectile-mode)
  :bind
  ("s-n" . counsel-projectile-switch-project)
  ("s-q" . projectile-replace))

(use-package counsel
  :ensure t
  :defer t
  :diminish ivy-mode
  :ensure smex
  :ensure flx
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
    '((t . ivy--regex-fuzzy)))
  :bind (("M-x" . counsel-M-x)
          ("C-d" . ivy-switch-buffer-kill)
          ("C-c b" . counsel-imenu)
          ("C-x C-f" . counsel-find-file)
          ("C-x b" . ivy-switch-buffer)
          ("C-h f" . counsel-describe-function)
          ("C-h v" . counsel-describe-variable)
          ("C-h b" . counsel-descbinds)
          ("M-SPC" . counsel-shell-history)))

(use-package editorconfig
  :diminish
  :ensure t
  :config
  (editorconfig-mode 1)
  (add-hook 'editorconfig-custom-hooks
    (lambda (hash) (setq web-mode-block-padding 0))))

;; Global autocompletion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :diminish
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2))

;;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; Flyspell
(use-package flyspell
  :ensure t
  :diminish
  :defer t
  :config
  (setq ispell-program-name "aspell"
    ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :diminish
  :config
  (volatile-highlights-mode +1))

;;; Node path
(use-package add-node-modules-path
  :ensure t
  :hook ((js2-mode . add-node-modules-path)
          (rjsx-mode . add-node-modules-path)
          (js-mode . add-node-modules-path)))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
          ("\\.html\\.erb\\'" . web-mode)
          ("\\.mustache\\'" . web-mode)
          ("\\.jinja\\'" . web-mode)
          ("\\.js\\'" . web-mode)
          ("\\.php\\'" . web-mode))
  :init
  (setq web-mode-engines-alist
    '(("\\.js\\'"  . "html")
       '("\\.jinja\\'"  . "django"))))

(use-package mmm-mode
  :ensure t
  :commands mmm-mode
  :config
  (setq
    mmm-global-mode 'buffers-with-submode-classes
    mmm-submode-decoration-level 0)
  (mmm-add-mode-ext-class 'js-mode "\\.js\\'" 'html)
  (mmm-add-classes
    '((js-html
        :submode html-mode
        :face mmm-declaration-submode-face
        :front "[^a-zA-Z]html`" ;; regex to find the opening tag
        :back "`"))) ;; regex to find the closing tag
  (mmm-add-mode-ext-class 'js-mode nil 'js-html)
  (setq mmm-global-mode 'maybe)
  (use-package mmm-auto))

;;; Yaml mode
(use-package yaml-mode
  :ensure t
  :defer t
  :mode (".yaml$"))

(use-package typescript-mode
  :ensure t)

(use-package yaml-tomato :ensure t)

(use-package nix-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'")

(use-package scss-mode
  :ensure t
  :defer t
  :mode ("\\.scss\\'")
  :config
  (autoload 'scss-mode "scss-mode")
  (setq scss-compile-at-save 'nil))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md$"))

(use-package markdown-mode+
  :ensure t
  :after markdown-mode)

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
          ("\\.tmpl\\'" . json-mode)
          ("\\.eslintrc\\'" . json-mode)))

(use-package format-all
  :ensure t
  :defer t)

(use-package shell-pop
  :ensure t
  :defer t
  :bind
  ("C-x t" . shell-pop)
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq term-buffer-maximum-size 0)
  (setq shell-pop-term-shell "/run/current-system/sw/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package logview
  :ensure t
  :mode ("syslog\\(?:\\.[0-9]+\\)" "\\.log\\(?:\\.[0-9]+\\)?\\'"))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package browse-at-remote
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(setq-default display-line-numbers 'directly
  display-line-numbers-width 3
  display-line-numbers-widen t)

(setq-default indicate-empty-lines t)

;;; Whitespace
(use-package whitespace
  :config
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :diminish whitespace ""
  :init
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '('tabs tab-mark)))(provide 'theme)

(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show trailing whitespace between t and nil."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; Mode line format
(use-package mood-line
  :ensure t
  :defer t)

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;;; init.el ends here
