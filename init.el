;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((normal-gc-cons-threshold (* 20 1024 1024))
       (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq package-enable-at-startup nil)
(require 'package)

;; Package Repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Show line number
;; (global-linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;; display
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(fset #'display-startup-echo-area-message #'ignore)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq mode-line-percent-position nil)

;; window configuration
(winner-mode t)

;; load mouse wheel
(mwheel-install)

;; doesn't show the nouse on the buffers
(mouse-avoidance-mode "animate")

;; delete the selected element if paste while it's selected
(delete-selection-mode 1)

;; move Between buffers using shift+arrow
(windmove-default-keybindings)

;; hightlight parenthesis that closes/opens the selected parenthesis
(show-paren-mode 1)

;; pairs
(setq electric-pair-pairs '(
                             (?\{ . ?\})
                             (?\( . ?\))
                             (?\[ . ?\])
                             (?\" . ?\")
                             ))
(electric-pair-mode t)

;; cursorline
(global-hl-line-mode t)

;; Basic: Turn off bugging yes-or-no-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Basic: Auto revert mode (Updating buffers when changed on disk)
(global-auto-revert-mode)

;;; Set the font
(set-face-attribute 'default nil
                    :family "Monospace"
                    :height 110
                    :weight 'normal
                    :width 'normal)

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
(setq-default indent-tabs-mode     nil
  tab-width            2
  truncate-lines       t
  line-move-visual     t
  indicate-empty-lines t)

;;; misc
(setq require-final-newline t
  delete-by-moving-to-trash t
  delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Set undo limits
(setq undo-limit (* 16 1024 1024))
(setq undo-strong-limit (* 24 1024 1024))
(setq undo-outer-limit (* 64 1024 1024))

;;; Do not disable commands
(setq disabled-command-function nil)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; subwords
(global-subword-mode 1)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (quote split-window-horizontally))
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; saveplace
(require 'saveplace)
(save-place-mode 1)

;; following windows splits
(defun split-and-follow-horizontally ()
  "."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x C-2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  "."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x C-3") 'split-and-follow-vertically)

(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)

;; Speed up large files such as SQL backups
(defun init-large-buffer ()
  "Setup large buffers to better handle large buffers."
  (when (> (buffer-size) large-file-warning-threshold)
    (setq buffer-read-only t)
    (buffer-disable-undo)))
(add-hook 'find-file-hook #'init-large-buffer)

(setq package-selected-packages
  '(async
     delight
     smartparens
     company
     flycheck
     flycheck-indicator
     helm
     helm-swoop
     helm-projectile
     helm-descbinds
     helm-ag
     projectile
     phi-search
     multiple-cursors
     rainbow-delimiters
     expand-region
     wrap-region
     editorconfig
     which-key
     shell-pop
     undo-tree
     linum-relative
     nix-mode
     markdown-mode
     scss-mode
     yaml-mode
     js-doc
     magit
     diff-hl
     solarized-theme
     simple-modeline
     god-mode
     ))
(package-install-selected-packages)

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

(require 'delight)
(delight '((eldoc-mode nil "eldoc")))
(delight '((whitespace-mode nil "whitespace")))

(require 'which-key)
(which-key-mode +1)

;; god mode
(require 'god-mode)
(god-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)
(global-set-key (kbd "<escape>") 'god-mode-all)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-0") #'delete-window)
(which-key-enable-god-mode-support)

(defun my-god-mode-update-cursor ()
  "."
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(add-hook 'god-mode-enabled-hook #'my-god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook #'my-god-mode-update-cursor)

;; relative numbers
(require 'linum-relative)
(setq linum-relative-current-symbol "")
(add-hook 'prog-mode-hook 'linum-relative-mode)

(require 'async)

(require 'dired)
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
(require 'dired-x)
(dired-async-mode)

;;; History saving
(require 'savehist)
(setq history-length 1024)
(setq history-delete-duplicates t)
(setq search-ring-max 1024)
(setq regexp-search-ring-max 1024)
(setq savehist-additional-variables '(extended-command-history file-name-history search-ring regexp-search-ring))
(setq savehist-file (expand-file-name ".savehist" user-emacs-directory))
(savehist-mode)

;; editorconfig
(require 'editorconfig)
(editorconfig-mode 1)
(delight '((editorconfig-mode nil "editorconfig")))

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; wrap-region
(require 'wrap-region)
(wrap-region-global-mode t)
(delight '((wrap-region-mode nil "wrap-region")))
(wrap-region-add-wrappers
  '(
     ("`" "`")
     ("*" "*")
     ))

;; Helm Mode
(require 'helm)
(delight 'helm-mode)
(require 'helm-config)
(helm-autoresize-mode 1)
(require 'helm-projectile)
(require 'helm-descbinds)
(require 'helm-ag)

;; projectile
(require 'projectile)
(setq projectile-require-project-root nil
  projectile-enable-caching t
  projectile-completion-system 'helm)
(projectile-global-mode)
(global-set-key (kbd "s-n") 'helm-projectile-switch-project)
(global-set-key (kbd "s-/") 'helm-projectile-switch-to-buffer)
(global-set-key (kbd "s-p") 'helm-projectile-find-file)
(global-set-key (kbd "s-o") 'projectile-switch-open-project)
(eval-after-load "projectile"
  '(setq projectile-mode-line
     '(:eval (list " [Pj:"
               (propertize (projectile-project-name)
                 'face '(:foreground "#81a2be"))))))

(helm-descbinds-mode)
(helm-projectile-on)

(with-eval-after-load "helm"
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-autoresize-max-height 0
  helm-autoresize-min-height 30
  helm-buffers-fuzzy-matching t
  helm-semantic-fuzzy-match t
  helm-imenu-fuzzy-match t
  helm-M-x-fuzzy-match t
  helm-display-header-line nil
  helm-split-window-in-side-p nil
  helm-move-to-line-cycle-in-source nil
  helm-ff-search-library-in-sexp t
  helm-scroll-amount 8)
(helm-mode 1)

;; eshell
;; use helm to list eshell history
(require 'helm-eshell)
(add-hook 'eshell-mode-hook
  #'(lambda ()
      (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

;; company-mode
(require 'company)
(setq company-idle-delay 0.3)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above t)
(add-hook 'after-init-hook 'global-company-mode)
(delight '((company-mode nil "company")))

;; magit
(require 'magit)
(setq magit-push-always-verify nil)
(setq git-commit-summary-max-length 50)
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)
(global-set-key (kbd "C-x g") 'magit-status)
(defun mu-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))
(eval-after-load 'magit-mode
  '(define-key magit-status-mode-map (kbd "q") #'mu-magit-kill-buffers))

;; git-gutter
(require 'diff-hl)
(global-diff-hl-mode +1)
(setq vc-git-diff-switches '("--histogram"))
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'prog-mode-hook #'diff-hl-mode)
(add-hook 'org-mode-hook #'diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)

(require 'flycheck-indicator)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-indicator-mode))
(setq flycheck-indicator-icon-error 9632)
(setq flycheck-indicator-icon-info 9679)
(setq flycheck-indicator-icon-warning 9650)
(setq flycheck-indicator-status-icons
  '((running . "◉")
     (errored . "◙")
     (finished . "●")
     (interrupted . "◘")
     (suspicious . "◘")
     (not-checked . "○")))

;; phi-search
(require 'phi-search)
(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)
(require 'phi-replace)
(global-set-key (kbd "M-%") 'phi-replace-query)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-.") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-,") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; shell-pop
(require 'shell-pop)
(setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
(setq term-buffer-maximum-size 0)
(setq shell-pop-term-shell "/run/current-system/sw/bin/bash")
(shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
(global-set-key (kbd "C-x t") 'shell-pop)

(defvar my-term-shell "/run/current-system/sw/bin/bash")
(defadvice ansi-term (before force-bash)
  "."
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)
(global-set-key (kbd "<C-S-return>") 'ansi-term)

;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-lazy-drawing nil)
(setq undo-tree-auto-save-history t)
(let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
  (setq undo-tree-history-directory-alist (list (cons "." undo-dir))))
(delight '((undo-tree-mode nil "undo-tree")))

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; nix-mode
(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(setq solarized-use-more-italic t)
(setq x-underline-at-descent-line t)
(load-theme 'solarized-gruvbox-dark t)
(require ' whitespace)
(delight '((whitespace-mode nil "wk")))
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'whitespace-mode))

(setq whitespace-style '('tabs tab-mark))
(provide 'theme)

(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show trailing whitespace between t and nil."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(require 'simple-modeline)
(add-hook 'after-init-hook 'simple-modeline-mode t)

(add-hook 'after-init-hook
  (lambda ()
    (require 'server)
    (unless (server-running-p)
      (server-start))))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
