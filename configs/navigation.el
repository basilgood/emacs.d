;;;  navigation.el --- navigation configs
;;; commentary:
;;; code:

(use-package async
  :ensure t
  :init (dired-async-mode 1))

(use-package projectile
  :ensure t
  :bind (("C-x p s" . projectile-switch-open-project)
	 ("C-x p" . projectile-switch-project))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package anzu
  :ensure t
  :config
 (global-anzu-mode +1)
  (set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)
  (custom-set-variables
    '(anzu-mode-lighter "")
    '(anzu-deactivate-region t)
    '(anzu-search-threshold 1000)
    '(anzu-replace-threshold 50)
    '(anzu-replace-to-string-separator " => ")))

(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)

(use-package avy
  :ensure t
  :bind
    ("M-s" . avy-goto-char-2))

(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-prefix nil
    ido-enable-flex-matching t
    ido-case-fold nil
    ido-auto-merge-work-directories-length -1
    ido-create-new-buffer 'always
    ido-use-filename-at-point nil
    ido-max-prospects 15))

;; Try out flx-ido for better flex matching between words
(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1))

;; flx-ido looks better with ido-vertical-mode
(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (setq ido-use-faces t)
  (set-face-attribute 'ido-vertical-first-match-face nil
    :background nil
    :foreground "orange")
  (set-face-attribute 'ido-vertical-only-match-face nil
    :background nil
    :foreground nil)
  (set-face-attribute 'ido-vertical-match-face nil
    :foreground nil))

(use-package dash
  :ensure t)

(defun my/ido-go-straight-home ()
  (interactive)
  (cond
   ((looking-back "~/") (insert "Projects/"))
   ((looking-back "/") (insert "~/"))
   (:else (call-interactively 'self-insert-command))))

(defun my/setup-ido ()
  ;; Go straight home
  (define-key ido-file-completion-map (kbd "~") 'my/ido-go-straight-home)

  ;; Use C-w to go back up a dir to better match normal usage of C-w
  ;; - insert current file name with C-x C-w instead.
  (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)

  (define-key ido-file-dir-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-dir-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name))

(add-hook 'ido-setup-hook 'my/setup-ido)

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "bower_components")
(add-to-list 'ido-ignore-directories "node_modules")

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))

;;; navigation ends here
