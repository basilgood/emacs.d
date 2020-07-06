;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)
;;;;  straight.el
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;  package.el
;;; so package-list-packages includes them
;; (require 'package)
;; (add-to-list 'package-archives
;;             '("melpa" . "https://melpa.org/packages/"))

;; faster emacs start-up
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq ad-redefinition-action 'accept)

(use-package no-littering
  :demand t)
(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)
(use-package bind-key
  :demand t)

;; Supress GUI features
(setq inhibit-startup-message t)

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; Disable cursor blink
(blink-cursor-mode 0)

;; smooth scroll & friends
(setq mouse-wheel-progressive-speed nil
      scroll-conservatively 10000
      scroll-preserve-screen-position 'always
      auto-window-vscroll nil)

;; DONT move points out of eyes
(setq mouse-yank-at-point t)

;; No tabs
(setq-default tab-width 2
              indent-tabs-mode nil)

(prefer-coding-system 'utf-8)

;; font
;; (set-face-attribute 'default nil :family "DejaVuSansMono Nerd Font" :height 135)
(set-face-attribute 'default nil :family "Monospace" :height 108)

;; Paragraphs
(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq-default line-spacing 1)

;;; Macros
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'split-horizontal-2-3
      [?\C-x ?3 ?\C-u ?1 ?5 ?\M-x ?e ?n ?l ?a ?r tab ?- ?h ?o ?r tab return])

;; Highlight parenthesises
(use-package paren
  :ensure nil
  :config (show-paren-mode)
  :init
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; The selected region of text can be deleted
(use-package delsel
  :ensure nil
  :config (delete-selection-mode))

(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings)
  (when (fboundp 'windmove-display-default-keybindings)
    (windmove-display-default-keybindings))
  (when (fboundp 'windmove-delete-default-keybindings)
    (windmove-delete-default-keybindings)))

(use-package winner
  :ensure nil
  :config
  (winner-mode +1))

(use-package transpose-frame)
(use-package buffer-move)
(use-package ibuffer
  :ensure nil
  :bind (([remap list-buffers] . #'ibuffer)))

;; mode-line
(line-number-mode)
(column-number-mode)
(setq mode-line-percent-position nil)

(global-display-line-numbers-mode)
(setq-default display-line-numbers 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 3
              display-line-numbers-widen t)
(set-face-attribute 'line-number nil
                    :font "Monospace"
                    :background "default" :foreground "#5f5e6b")
(set-face-attribute 'line-number-current-line nil
                    :font "Monospace"
                    :background "default" :foreground "yellow")

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (quote split-window-horizontally))
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; following windows splits
(defun split-and-follow-horizontally ()
  "."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  "."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; save cursor position
(use-package saveplace
  :ensure nil
  :config
  (save-place-mode +1))

;; highlight current line
(use-package hl-line
  :ensure nil
  :config (global-hl-line-mode))

;; Tips for next keystroke
(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay most-positive-fixnum)
  (setq which-key-idle-secondary-delay 1e-100)
  (which-key-mode +1)
  :blackout t)

(use-package vterm
  :straight nil)

(use-package shell-pop
  :config
  (setq shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm shell-pop-term-shell)))))
  (setq term-buffer-maximum-size 0)
  (setq shell-pop-term-shell "/run/current-system/sw/bin/bash")
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  :bind
  (("C-x t" . shell-pop)))


(defvar my-term-shell "/run/current-system/sw/bin/bash")
(defadvice vterm (before force-bash)
  "."
  (interactive (list my-term-shell)))
(ad-activate 'vterm)
(global-set-key (kbd "<C-S-return>") 'vterm)

(use-package async)
(use-package dired
  :straight nil
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
  :straight nil
  :init
  (setq ido-enable-flex-matching t
        ido-virtual-buffers t
        ido-use-faces t
        ido-auto-merge-work-directories-length -1
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-everywhere 1)
  :config
  (ido-mode 1))
(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))
(use-package ido-vertical-mode
  :after ido
  :init
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  :config
  (ido-vertical-mode 1))
(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode))
(use-package amx
  :config
  (amx-mode 1))

(use-package projectile
  :config
  (progn
    (setq projectile-completion-system 'ido)
    (projectile-global-mode)))

(use-package flycheck
  :defines flycheck-mode-hook
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

(global-set-key (kbd "s-n") 'projectile-switch-project)
(global-set-key (kbd "s-/") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-p") 'projectile-find-file)
(global-set-key (kbd "s-o") 'projectile-switch-open-project)

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package diff-hl
  :init
  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package company
  :blackout t
  :config
  (setq company-idle-delay 0.3)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package multiple-cursors
  :bind
  (("C-c n" . mc/mark-next-like-this)
   ("C-c p" . mc/mark-previous-like-this)))

(use-package expand-region
  :blackout t
  :bind
  (("C-=" . er/expand-region)))

(use-package wrap-region
  :blackout t
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers
   '(
     ("`" "`")
     ("*" "*")
     )))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package nix-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))

(use-package server
  :ensure nil
  :config (server-mode))

;; show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(tabs tab-mark))
  (provide 'theme))

;; theme
(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat))
  :defer t
  :ensure t)

;; smart-mode-line
(use-package mood-line
  :config
  (mood-line-mode))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
