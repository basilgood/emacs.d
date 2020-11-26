;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)

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

(straight-use-package 'use-package)
(setq  straight-use-package-by-default t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq ad-redefinition-action 'accept)

(use-package no-littering)

(use-package package
  :straight nil
  :custom
  (package-quickstart t)
  (package-quickstart-file (no-littering-expand-var-file-name "package-quickstart.el")))

(use-package bind-key)

;; backup files
(setq make-backup-files nil
  auto-save-default nil)

;; cursor blink
(blink-cursor-mode 0)

;; smooth scroll & friends
(setq mouse-wheel-progressive-speed nil
  scroll-conservatively 10000
  scroll-preserve-screen-position 'always
  auto-window-vscroll nil)

;; DONT move points out of eyes
(setq mouse-yank-at-point t)

;; misc
(setq-default inhibit-startup-message t
  initial-scratch-message ";; <3 "
  inhibit-startup-message t
  inhibit-startup-screen t
  backward-delete-char-untabify-method nil
  window-combination-resize t
  truncate-lines t
  tab-width 2
  indent-tabs-mode nil)

(global-auto-revert-mode 1)

(use-package display-line-numbers
  :straight nil
  :hook
  ((prog-mode yaml-mode systemd-mode) . display-line-numbers-mode)
  :init
  (setq display-line-numbers-type 'absolute
    display-line-numbers-width-start t))

(use-package hl-line
  :straight nil
  :hook
  (after-init . global-hl-line-mode))
;; (with-eval-after-load 'display-line-numbers
;;   (setq display-line-numbers-type 'none
;;         display-line-numbers-width-start t))

(prefer-coding-system 'utf-8)
(setq enable-recursive-minibuffers t)
(setq case-fold-search nil)

;; font
(setq default-frame-alist '((font . "DejaVuSansMono Nerd Font-11:weight=book:width=regular")))

;; Paragraphs
(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;; Macros
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'split-horizontal-2-3
  [?\C-x ?3 ?\C-u ?1 ?5 ?\M-x ?e ?n ?l ?a ?r tab ?- ?h ?o ?r tab return])

(use-package paren
  :straight nil
  :hook
  (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t))

(use-package delsel
  :straight nil
  :hook
  (after-init . delete-selection-mode))

(use-package window :disabled
  :no-require
  :init
  (setq display-buffer-alist
   '((".*"
      (display-buffer-reuse-window display-buffer-same-window)
      (reusable-frames . t))))
  (even-window-sizes t))

(use-package windmove
  :straight nil
  :config (windmove-default-keybindings))

(use-package winner
  :straight nil
  :config
  (winner-mode +1))

(use-package transpose-frame)
(use-package buffer-move)
(use-package ibuffer
  :bind (([remap list-buffers] . #'ibuffer)))

;; mode-line
(line-number-mode)
(column-number-mode)
(setq mode-line-percent-position nil)

;; ediff
(use-package ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-show-clashes-only t)
  (ediff-diff-options "-w")
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

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
  :hook
  (after-init . save-place-mode))

(global-unset-key (kbd "C-z"))

(use-package undo-tree
  :defer t
  :init
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-history-directory-alist
    `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1))

(use-package flyspell
  :straight nil
  :custom
  (flyspell-mode-map (make-sparse-keymap) "Disable all flyspell bindings")
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra")))

(use-package flyspell-correct
  :defines flyspell-mode-map
  :requires flyspell
  :config (bind-key [remap ispell-word] #'flyspell-correct-wrapper flyspell-mode-map))

(use-package auto-correct
  :hook (flyspell-mode . auto-correct-mode)
  :custom (flyspell-use-global-abbrev-table-p t))

;; Tips for next keystroke
(use-package which-key
  :defer t
  :init
  (which-key-mode +1))

(use-package vterm
  :straight nil
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
  ;; :straight (:host github :repo "raxod502/selectrum")
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
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

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

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package git-gutter
  :init
  (global-git-gutter-mode +1))

(global-set-key (kbd "M-g M-g") 'hydra-git-gutter/body)


(use-package git-timemachine
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
  :init
  (global-company-mode)
  (setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))
  :config
  (setq company-idle-delay 0.3)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t))

(use-package lsp-mode
  :commands (lsp lsp-execute-code-action)
  :hook ((js-mode . lsp-deferred)
          (web-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-diagnostics-modeline-mode))
  :bind ("C-c C-c" . #'lsp-execute-code-action)
  :config
  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.

  ;; ..
  )

(use-package lsp-ui
  :custom
  (lsp-ui-doc-delay 0.75)
  (lsp-ui-doc-max-height 200)
  :after lsp-mode)

;; (use-package lsp-ui
;;   :requires lsp-mode flycheck
;;   :config

;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-use-childframe t
;;         lsp-ui-doc-position 'top
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-sideline-enable nil
;;         lsp-ui-flycheck-enable t
;;         lsp-ui-flycheck-list-position 'right
;;         lsp-ui-flycheck-live-reporting t
;;         lsp-ui-peek-enable t
;;         lsp-ui-peek-list-width 60
;;         lsp-ui-peek-peek-height 25)

;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends)

   ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

(use-package multiple-cursors
  :bind (("C-<"     . mc/mark-previous-like-this)
          ("C->"     . mc/mark-next-like-this)
          ("C-+"     . mc/mark-next-like-this)
          ("C-c C-<" . mc/mark-all-like-this))
  :config
  ;; From active region to multiple cursors:
  (general-define-key
    :preface "C-c m"
    "r" 'set=rectangular-region-anchor
    "c" 'mc/edit-lines
    "e" 'mc/edit-ends-of-lines
    "a" 'mc/edit-beginnings-of-lines))

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
       )))

(use-package ctrlf
  :init
  (ctrlf-mode +1))

(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package web-mode
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
  (setq mmm-global-mode 'maybe))

(use-package add-node-modules-path
  :hook ((js2-mode . add-node-modules-path)
          (rjsx-mode . add-node-modules-path)
          (js-mode . add-node-modules-path)))

(use-package js-mode
  :straight nil
  :hook (js-mode . flycheck-mode))
(use-package toml-mode)
(use-package yaml-mode)
(use-package nix-shell
  :straight nil
  :no-require t
  :init
  (defun nix-shell ()
    (interactive)
    (let ((explicit-shell-file-name "shell.nix")
          (explicit-shell-args nil))
      (call-interactively 'shell))))
(use-package nix-mode)
(use-package nginx-mode)
(use-package json-mode)
(use-package markdown-mode)

(use-package server
  :straight nil
  :config (server-mode))
(use-package whitespace
  :defer t
  :config
  (setq whitespace-style '(face empty trailing tabs tab-mark))
   :hook
  (prog-mode . whitespace-mode))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-opera t)
  (doom-themes-org-config))

(use-package mood-line
  :demand t
  :config
  (mood-line-mode))

(provide 'init)

;;; init.el ends here
