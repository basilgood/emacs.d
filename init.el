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
(setq  straight-use-package-by-default t)
(setq use-package-always-defer t)

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
(setq-default inhibit-startup-message t
              initial-scratch-message ";; <3 "
              inhibit-startup-message t
              inhibit-startup-screen t
              backward-delete-char-untabify-method nil
              window-combination-resize t
              tab-width 2
              indent-tabs-mode nil)

(prefer-coding-system 'utf-8)
(setq enable-recursive-minibuffers t)

;; font
(set-face-attribute 'default nil :family "Monospace" :height 120)

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
  :demand t
  :config (show-paren-mode)
  :init
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; The selected region of text can be deleted
(use-package delsel
  :hook ((prog-mode text-mode) . delete-selection-mode))

(use-package windmove
  :config
  (windmove-default-keybindings)
  (when (fboundp 'windmove-display-default-keybindings)
    (windmove-display-default-keybindings))
  (when (fboundp 'windmove-delete-default-keybindings)
    (windmove-delete-default-keybindings)))

(use-package winner
  :demand t
  :config
  (winner-mode +1))

(use-package transpose-frame)
(use-package buffer-move)
(use-package ibuffer
  :demand t
  :bind (([remap list-buffers] . #'ibuffer)))

;; mode-line
(line-number-mode)
(column-number-mode)
(setq mode-line-percent-position nil)

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

(defvar my-term-shell "/run/current-system/sw/bin/bash")
(defadvice vterm (before force-bash)
  "."
  (interactive (list my-term-shell)))
(ad-activate 'vterm)
(global-set-key (kbd "<C-S-return>") 'vterm)

;; save cursor position
(use-package saveplace
  :demand t
  :config
  (save-place-mode +1))

;; highlight current line
(use-package hl-line
  :demand t
  :config (global-hl-line-mode))

;; Tips for next keystroke
(use-package which-key
  :config
  (which-key-mode +1)
  :blackout t)

(use-package vterm
  :demand t
  :straight nil)

(use-package shell-pop
  :config
  (setq shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm shell-pop-term-shell)))))
  (setq term-buffer-maximum-size 0)
  (setq shell-pop-term-shell "/run/current-system/sw/bin/bash")
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  :bind
  (("C-x t" . shell-pop)))

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

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :defer t
  :init
  (selectrum-mode +1))

(use-package prescient
  :config
  (prescient-persist-mode +1)
  (setq prescient-history-length 1000))

(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("selectrum-prescient.el"))
  :demand t
  :after selectrum
  :config
  (selectrum-prescient-mode +1))

(use-package projectile
  :bind-keymap* (("C-c p" . projectile-command-map))
  :bind (("s-n" . projectile-switch-project)
         ( "s-/" . projectile-switch-to-buffer)
         ("s-p" . projectile-find-file)
         ("s-o" . projectile-switch-open-project))
  :config
  (setq projectile-completion-system 'default)
  (projectile-mode +1))

(use-package flycheck
  :demand t
  :config
  (setq flycheck-completion-system 'default
        flycheck-idle-change-delay 1.0
        flycheck-indication-mode 'left-fringe)
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
  (global-flycheck-mode))

(use-package hydra
  :ensure hydra
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

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(setq magit-status-margin
      '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

(global-set-key (kbd "M-g M-g") 'hydra-git-gutter/body)


(use-package git-timemachine
  :ensure t
  )
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

(use-package company
  :demand t
  :blackout t
  :init (setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))
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
  :demand t
  :blackout t
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers
   '(
     ("`" "`")
     ("*" "*")
     )))

(use-package ctrlf
  :demand t
  :config
  (ctrlf-mode +1))

(use-package editorconfig
  :blackout t
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
  (provide 'theme)
  :blackout t)

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
  :demand t
  :config
  (mood-line-mode))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
