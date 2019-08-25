;;; init.el --- Emacs configuration
;;; Commentary:
;;; Code:
;; (require 'emacs-load-time)

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(eval-and-compile
  (defun emacs-path (path)
    (expand-file-name path user-emacs-directory)))

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(use-package quelpa-use-package
    :ensure t
    :init
  (setq quelpa-update-melpa-p nil))

;;; Set window size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 180 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
        (add-to-list 'default-frame-alist (cons 'width 180))
      (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
                 (cons 'height (/ (- (x-display-pixel-height) 200) (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;;; Disable GUI stuff
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;;; Start with empty scratch buffer
(fset #'display-startup-echo-area-message #'ignore)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

;;; Set the font
(set-face-attribute 'default nil :family "Monospace" :height 100)

;;; Disable lock files
(setq create-lockfiles nil)

;;; Disable backup files
(setq make-backup-files nil)

;;; Move auto-save files to saner location
(let ((auto-save-dir (file-name-as-directory (expand-file-name "autosave" user-emacs-directory))))
  (setq auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-dir))
  (setq auto-save-file-name-transforms (list (list ".*" (replace-quote auto-save-dir) t))))

;;; Use UTF-8
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;;; Fix scrolling
(setq mouse-wheel-progressive-speed nil)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 'always)
(setq auto-window-vscroll nil)

;;; Clipboard
(setq-default select-active-regions nil)
(when (boundp 'x-select-enable-primary)
  (setq x-select-enable-primary nil))

;;; Set undo limits
(setq undo-limit (* 16 1024 1024))
(setq undo-strong-limit (* 24 1024 1024))
(setq undo-outer-limit (* 64 1024 1024))

;;; Use spaces
(setq indent-tabs-mode nil)
(setq tab-width 2)

;;; Do not disable commands
(setq disabled-command-function nil)

;;; Disable electrict indent
(when (bound-and-true-p electric-indent-mode)
  (electric-indent-mode -1))

;;; Ignore case for completion
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; misc
(setq sentence-end-double-space 'nil
      require-final-newline t
      truncate-lines t
      delete-by-moving-to-trash t)
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Narrow to region
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(use-package diminish
  :ensure t
  :defer 3)

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
  (savehist-mode))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-mode))

(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode))

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

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  (progn
    (setq org-ellipsis " ")
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
    (setq org-confirm-babel-evaluate nil)
    (setq org-export-with-smart-quotes t)
    (setq org-src-window-setup 'current-window)
    (add-hook 'org-mode-hook 'org-indent-mode)))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "," 'other-window
    "." 'mode-line-other-buffer
    "b" 'counsel-switch-buffer
    "f" 'counsel-find-file
    "k" 'kill-this-buffer
    "w" 'save-buffer
    "x" 'evil-window-delete
    "a" 'align-regexp
    "m" 'smerge-command-prefix
    "t" 'shell-pop
    "h" 'hydra-projectile/body
    ))

(use-package evil
  :ensure t
    :preface
  (defun split-window-vertically-and-switch ()
    (interactive)
    (split-window-vertically)
    (other-window 1))

  (defun split-window-horizontally-and-switch ()
    (interactive)
    (split-window-horizontally)
    (other-window 1))
  :config
  (evil-mode)
  :init
  (setq evil-search-module 'evil-search)
  (mapc (lambda (m) (add-to-list 'evil-emacs-state-modes m t))
    '(eshell-mode
       calendar-mode
       finder-mode
       info-mode
       dired-mode
       image-mode
       image-dired-thumbnail-mode
       image-dired-display-image-mode
       git-rebase-mode
       help-mode
       sql-interactive-mode
       org-capture-mode))
  (evil-set-initial-state 'term-mode 'emacs)
  (setq evil-emacs-state-cursor  '("red" box))
  (setq evil-normal-state-cursor '("gray" box))
  (setq evil-visual-state-cursor '("gray" box))
  (setq evil-insert-state-cursor '("gray" bar))
  (setq evil-motion-state-cursor '("gray" box))
  (define-key evil-normal-state-map  (kbd "<backspace>") 'projectile-switch-to-buffer)
  (define-key evil-normal-state-map  (kbd "-") 'dired-jump)
  (define-key evil-normal-state-map  (kbd "gb") 'browse-at-remote)
  (define-key evil-normal-state-map  (kbd "gs") 'magit-status)
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-normal-state-map  (kbd "gt") 'git-timemachine-toggle)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")))

(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . "")
  :bind (:map undo-tree-map ("C-x u" . hydra-undo-tree/body))
  :init (defhydra hydra-undo-tree (:hint nil)
          "
  _p_: undo  _n_: redo _s_: save _l_: load   "
          ("p"   undo-tree-undo)
          ("n"   undo-tree-redo)
          ("s"   undo-tree-save-history)
          ("l"   undo-tree-load-history)
          ("u"   undo-tree-visualize "visualize" :color blue)
          ("q"   nil "quit" :color blue))
  :config
  (global-undo-tree-mode))

(use-package evil-commentary
  :ensure t
  :diminish evil-commentary ""
  :config
  (evil-commentary-mode))

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-ex-match))

;;; Which key
(use-package which-key
  :ensure t
  :defer 2
  :diminish
  :config
  (which-key-mode))

;;; Navigation
(use-package async
  :defer 2
  :ensure t)

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq
   dired-recursive-deletes 'always
   dired-recursive-copies 'always
   dired-dwim-target t
   dired-listing-switches "-alhv --group-directories-first"
   dired-no-confirm '(copy))
  (require 'dired-x)
  (dired-async-mode))

(use-package ag
  :defer 2
  :ensure t)

(use-package perspective
  :ensure t
  :defer 2
  :commands persp-mode
  :config
  (persp-mode))

;; (use-package projectile
;;   :ensure t
;;   :diminish
;;   :config
;;   (projectile-mode)
;;   :init
;;   (setq projectile-require-project-root nil
;;         projectile-enable-caching t
;;         projectile-completion-system 'ivy)
;;   :bind
;;   ("M-g a" . hydra-projectile/body)
;;   ("s-n" . counsel-projectile-switch-project)
;;   ("s-p" . projectile-find-file)
;;   ("s-g" . projectile-ag)
;;   ("s-q" . projectile-replace))

(use-package prescient
  :defer t
  :ensure t)

(use-package counsel
  :ensure t
  :defer t
  :diminish ivy-mode
  :config
  (use-package smex :defer t :ensure t)
  (use-package flx :defer t :ensure t)
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  :bind (("M-x" . counsel-M-x)
         ("C-c b" . counsel-imenu)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("C-k" . ivy-switch-buffer)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h b" . counsel-descbinds)
         ("M-SPC" . counsel-shell-history)))

(use-package ivy-prescient
  :ensure t
  :after ivy
  :config (ivy-prescient-mode))

(use-package swiper
  :ensure t
  :defer t
  :bind* ("C-s" . swiper))

;; (use-package counsel-projectile
;;   :ensure t
;;   :defer t
;;   :bind ("C-x r R" . counsel-projectile-ag)
;;   :config
;;   (add-hook 'text-mode-hook 'counsel-projectile-mode)
;;   (add-hook 'prog-mode-hook 'counsel-projectile-mode))

(use-package ibuffer
  :ensure t
  :defer t
  :ensure ibuffer-tramp
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("M-o"     . nil)) ;; unbind ibuffer-visit-buffer-1-window
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-tramp-set-filter-groups-by-tramp-connection)
              (ibuffer-do-sort-by-alphabetic))))

;;; FZF
(defun my-lcd ()
  (interactive)
  (fzf/start default-directory
             (fzf/grep-cmd "lcd" "-l %s")))

(use-package fzf
  :ensure t
  :defer t
  :init
  (autoload 'fzf/start "fzf")
  :bind
  (("C-c f" . fzf)
   ("C-c d" . my-lcd)))

;;; Expand region
(use-package expand-region
  :ensure t
  :defer t)

;;; Editorconfig
(use-package editorconfig
  :diminish
  :ensure t
  :config
  (editorconfig-mode 1)
  (add-hook 'editorconfig-custom-hooks
    (lambda (hash) (setq web-mode-block-padding 0))))

;;; Aggressive indent
(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'org-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'js-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode))

;;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "] e") 'flycheck-next-error)
  (define-key evil-normal-state-map (kbd "[ e") 'flycheck-previous-error)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; Flyspell
(use-package flyspell
  :ensure t
  :diminish flyspell-mode "s"
  :defer t
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer 2
  :config
  (rainbow-delimiters-mode))

;;; Completion
(use-package company-prescient
  :ensure t
  :hook (company-mode . company-prescient-mode))

(use-package company-posframe
  :ensure t
  :diminish
  :hook (company-mode . company-posframe-mode))

(use-package company
  :ensure t
  :diminish (company-mode . " ς")
  :init
  (setq company-minimum-prefix-length 2
        company-require-match 0
        company-selection-wrap-around t
        company-tooltip-limit 10
        company-show-numbers nil
        company-idle-delay 0.5)
  (setq company-dabbrev-ignore-buffers "\\.pdf\\'"
        company-dabbrev-downcase nil
        company-dabbrev-code-modes t
        company-dabbrev-code-other-buffers 'all
        company-dabbrev-other-buffers 'all
        company-dabbrev-code-everywhere t)
   :config
  (setq company-backends
        '((;; generic backends
           company-files          ; files & directory
           company-keywords       ; keywords
           company-dabbrev-code   ; code words
           company-dabbrev        ; words
           ;; code backends
           ;; company-elisp          ; emacs-lisp code
           ;; company-shell       ; shell
           ;; company-rtags          ; rtags
           ;; company-ycmd           ; ycmd
           ;; tag backends
           ;; company-etags          ; etags
           ;; company-gtags          ; gtags
           ;; completion at point
           company-capf)))
  (global-company-mode))

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
  (use-package mmm-auto))

;;; Yaml mode
(use-package yaml-mode
  :ensure t
  :defer t
  :mode (".yaml$"))

(use-package yaml-tomato :ensure t)

(use-package nix-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'")

(use-package vimrc-mode
  :ensure t
  :mode ("^\\.vimrc\\'"))

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

(use-package polymode
  :ensure t
  :ensure poly-markdown)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp)
  :config
  (require 'lsp-clients)
  (setq rust-format-on-save t)
  (use-package flycheck-rust
    :after flycheck
    :commands flycheck-rust-setup
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package cargo
  :ensure t
  :commands cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.tmpl\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode)))

(use-package format-all
  :ensure t
  :defer t)

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :after evil
  :config
  (pdf-tools-install)
  (progn
    (add-to-list 'evil-emacs-state-modes 'pdf-outline-buffer-mode)
    (add-to-list 'evil-emacs-state-modes 'pdf-view-mode))
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))
  (setq pdf-view-resize-factor 1.1)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))

(use-package org-pdfview
  :ensure t
  :defer t)

(use-package ssh-config-mode
  :ensure t
  :mode ("/\\.ssh/config\\'" "/system/ssh\\'" "/sshd?_config\\'" "/known_hosts\\'" "/authorized_keys2?\\'")
  :hook (ssh-config-mode . turn-on-font-lock)
  :config
  (autoload 'ssh-config-mode "ssh-config-mode" t))

(use-package shell-pop
  :ensure t
  :defer t
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/run/current-system/sw/bin/bash")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'org-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'js-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode))

(use-package logview
  :ensure t
  :mode ("syslog\\(?:\\.[0-9]+\\)" "\\.log\\(?:\\.[0-9]+\\)?\\'"))

;;; Highlight escape
(use-package highlight-escape-sequences
  :ensure t
  :config
  (hes-mode))

;;; Vagrant
(use-package vagrant
  :ensure t
  :defer t
  :commands (vagrant-destroy
             vagrant-edit
             vagrant-halt
             vagrant-provision
             vagrant-reload
             vagrant-resume
             vagrant-ssh
             vagrant-status
             vagrant-suspend
             vagrant-up))

;;; Wgrep
(use-package wgrep
  :ensure t
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package ag
  :ensure t
  :defer t
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :bind
  ("M-s a" . ag-project)
  :config
   (use-package wgrep-ag :ensure t))

;;; Git
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode)
  :custom
  (git-gutter:modified-sign "~")		; 
  (git-gutter:added-sign    "+")		; 
  (git-gutter:deleted-sign  "-")		; 
  :custom-face
  (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
  (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
  (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6"))))
  :config
  (define-key evil-normal-state-map (kbd "] c") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "[ c") 'git-gutter:previous-hunk)
  (evil-leader/set-key "g a" 'git-gutter:stage-hunk)
  (evil-leader/set-key "g r" 'git-gutter:revert-hunk))

(use-package magit
  :ensure t
  :defer t)

(use-package github-pullrequest
  :ensure t
  :defer t)

(use-package browse-at-remote
  :ensure t
  :defer t)

(use-package smerge-mode
  :ensure t
  :defer t
  :diminish
  :hook ((buffer-list-update . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))))

;;; Set the theme
(use-package tomorrow-theme
  :defer t
  :ensure color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-eighties 'no-confirm))

(setq-default display-line-numbers 'directly
              display-line-numbers-width 3
              display-line-numbers-widen t)

(set-face-attribute 'line-number nil
                    :font "DejaVu Sans Mono-9"
                    :background "undefined" :foreground "#5c6370")

(set-face-attribute 'line-number-current-line nil
                    :font "DejaVu Sans Mono-9"
                    :background "undefined" :foreground "#55cccc")

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
