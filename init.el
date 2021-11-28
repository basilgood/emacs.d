;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; -*- coding: utf-8; lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; better-defaults
(setq confirm-kill-processes nil)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default require-final-newline t)
(setq-default truncate-lines t)
(delete-selection-mode t)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(global-auto-revert-mode t)
(global-prettify-symbols-mode)
(global-unset-key (kbd "C-z"))
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-face-attribute 'default nil
                    :family "DejaVuSansMono Nerd Font"
                    :height 110)
(setq default-frame-alist '((font . "DejaVuSansMono Nerd Font-11")))

(defconst emacs-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p emacs-savefile-dir)
  (make-directory emacs-savefile-dir))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq tab-always-indent 'complete)
(global-set-key (kbd "M-/") #'hippie-expand)
(put 'erase-buffer 'disabled nil)

(defvar my-term-shell "/run/current-system/sw/bin/bash")
(defadvice vterm (before force-bash)
  "."
  (interactive (list my-term-shell)))
(ad-activate 'vterm)
(global-set-key (kbd "<C-S-return>") 'vterm)

;;; built-in packages
(use-package display-line-numbers
  :ensure nil
  :init
  (defun my/display-line-numbers ()
    (setq display-line-numbers-width-start t
          display-line-numbers-grow-only t
          display-line-numbers 'absolute))
  (defun display-line-numbers-equalize ()
    (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))
  :hook
  (prog-mode . display-line-numbers-equalize)
  (prog-mode . my/display-line-numbers)
  (org-mode . display-line-numbers-equalize)
  (org-mode . my/display-line-numbers))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode +1))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package saveplace
  :ensure nil
  :config
  (setq save-place-file (expand-file-name "saveplace" emacs-savefile-dir))
  (save-place-mode +1))

(use-package savehist
  :ensure nil
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" emacs-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file (expand-file-name "recentf" emacs-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package winner
  :ensure nil
  :config
  (winner-mode +1))

(use-package which-key
  :defer t
  :init
  (which-key-mode +1))

(use-package undo-tree
  :defer t
  :init
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-history-directory-alist
    `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1))

(use-package shell-pop
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq term-buffer-maximum-size 0)
  (setq shell-pop-term-shell "/run/current-system/sw/bin/bash")
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  :bind
  (("C-x t" . shell-pop)))

(use-package async)
(use-package dired
  :ensure nil
  :init (dired-async-mode 1)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq
    dired-recursive-deletes 'always
    dired-recursive-copies 'always
    dired-dwim-target t
    dired-listing-switches "-alhv --group-directories-first"
    dired-no-confirm '(copy))
  (define-key dired-mode-map (kbd "<left>") 'dired-up-directory)
  (define-key dired-mode-map (kbd "<right>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
  (require 'dired-x))

(use-package ido
  :ensure nil
  :init (ido-mode 1)
  :config
  (setq
   ido-enable-flex-matching 'nil
   ido-create-new-buffer 'always
   ido-everywhere t))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode t)
  :custom
  (ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (ido-vertical-indicator " →"))

(use-package flx-ido
  :config
  (flx-ido-mode t))

(use-package ido-completing-read+
  :init
  (ido-ubiquitous-mode 1))

(use-package amx
  :init
  (amx-mode 1))

(use-package projectile
  :init
  (setq projectile-project-search-path '("~/Projects/" "~/.emacs.d"))
  :bind-keymap* (("C-c p" . projectile-command-map))
  :bind (("s-\." . projectile-switch-project)
          ( "s-/" . projectile-switch-to-buffer)
          ("s-p" . projectile-find-file)
          ("s-\," . projectile-switch-open-project))
  :config
  (setq projectile-completion-system 'default)
  (projectile-mode +1))

;;; Completion
(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (bind-keys :map company-active-map
             ("<tab>" . company-indent-or-complete-common)
             ("C-<Tab>" . company-complete-selection))
  (global-company-mode))

;;; Lint
(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode nil)
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

;;; LSP
(use-package eglot
  :demand
  :hook ((js-mode typescript-mode typescript-react-mode) . eglot-ensure)
  :custom
  (eglot-confirm-server-initiated-edits . nil)
  :config
  (put 'typescript-react-mode 'eglot-language-id "typescriptreact")
  (add-to-list 'eglot-server-programs `(js-mode . ("typescript-language-server" "--stdio"))))
(setenv "PATH" (concat (getenv "PATH") ":/etc/profiles/per-user/vasy/bin"))
(setq exec-path (append exec-path '("/etc/profiles/per-user/vasy/bin")))

(use-package vterm
  :ensure nil
  :config
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  :hook (vterm-mode . turn-off-chrome))

(use-package vterm-toggle
  :custom
  (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
  (vterm-toggle-scope 'projectile)
  :bind (("C-c t" . #'vterm-toggle)
         :map vterm-mode-map
         ("s-t" . #'vterm) ; Open up new tabs quickly
         ))

(use-package shell-pop
  :config
  (setq shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm shell-pop-term-shell)))))
  (setq term-buffer-maximum-size 0)
  (setq shell-pop-term-shell "/run/current-system/sw/bin/bash")
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  :bind
  (("C-x t" . shell-pop)))

(use-package multiple-cursors
  :bind (("C-<"      . mc/mark-previous-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(use-package wrap-region
  :demand t
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers
    '(
       ("`" "`")
       ("*" "*")
       ("/* " " */" "#" (js-mode css-mode))
       )))

(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package pdf-tools
  :ensure nil
  :config
  (pdf-tools-install-noverify)

  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))
(use-package saveplace-pdf-view)

(use-package rg)
(use-package wgrep)

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :init
  (global-git-gutter-mode +1))

(global-set-key (kbd "M-g M-g") 'hydra-git-gutter/body)

(use-package git-timemachine)

(use-package hydra
  :init
  (global-set-key
    (kbd "C-x a")
    (defhydra toggle (:color blue)
      "toggle"
      ("a" abbrev-mode "abbrev")
      ("s" flyspell-mode "flyspell")
      ("d" toggle-debug-on-error "debug")
      ("f" auto-fill-mode "fill")
      ("t" toggle-truncate-lines "truncate")
      ("w" whitespace-mode "whitespace")
      ("q" nil "cancel"))))

(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                             :hint nil)
  "
  Git gutter:
    _j_: next hunk        _s_tage hunk     _q_uit
    _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
    ^ ^                   _p_opup hunk
    _h_: first hunk
    _l_: last hunk        set start _R_evision
  "
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
         (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
         (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
         ;; git-gutter-fringe doesn't seem to
         ;; clear the markup right away
         (sit-for 0.1)
         (git-gutter:clear))
    :color blue))

;;; Tree-sitter
(use-package tree-sitter
  :ensure t
  :hook ((go-mode . tree-sitter-hl-mode)
         (js-mode . tree-sitter-hl-mode)
         (typescript-mode . tree-sitter-hl-mode)
         (json-mode . tree-sitter-hl-mode)
         (python-mode . tree-sitter-hl-mode)
         (rust-mode . tree-sitter-hl-mode)
         (sh-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package js-mode
  :ensure nil
  :mode ("\\.mjs\\'" . js-mode)
  :hook ((js-mode . subword-mode)))

(use-package nix-mode)

(use-package add-node-modules-path
  :hook ((js2-mode . add-node-modules-path)
         (rjsx-mode . add-node-modules-path)
         (js-mode . add-node-modules-path)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

(use-package json-mode
  :mode (("\\.json$" . json-mode)))

;;; theme
(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package mood-line
  :config
  (mood-line-mode))

(use-package whitespace
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(tabs tab-mark))
  (setq whitespace-display-mappings
    '(
       (tab-mark     ?\t    [?\u21E5 ?\t] [?\\ ?\t])       ))
  (provide 'theme))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(provide 'init)
;;; init.el ends here
