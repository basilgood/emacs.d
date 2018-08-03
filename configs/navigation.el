(use-package async
  :ensure t
  :init (dired-async-mode 1))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-\,"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package projectile
  :ensure t
  :init
    (projectile-mode 1))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind (("C-c r"   . ivy-resume)
         ("C-x C-r" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-j" . ivy-immediate-done))
  :init (ivy-mode 1)
  :config
  (setq
   ivy-count-format "(%d/%d) "          ; Show current match and matches
   ivy-extra-directories nil            ; Do not show "./" and "../"
   ivy-virtual-abbreviate 'full         ; Show full file path
   ivy-height 10
    ivy-dynamic-exhibit-delay-ms 150
   ;; Jump back to first candidate when on the last one
   ivy-wrap t
   ;; Always ignore buffers set in `ivy-ignore-buffers'
   ivy-use-ignore-default 'always
   ;; Ignore some buffers in `ivy-switch-buffer'
   ivy-ignore-buffers '("company-statistics-cache.el" ".elfeed/index")))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
          ("C-x C-f" . counsel-find-file)))

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

;; (use-package swiper
;;   :ensure t
;;   :bind ("C-s" . 'swiper))

;; (use-package helm
;;   :ensure t
;;   :diminish helm-mode
;;   :bind
;;   ("C-x C-f" . 'helm-find-files)
;;   ("C-x C-b" . 'helm-buffers-list)
;;   ("M-x" . 'helm-M-x)
;;   :config
;;   (defun daedreth/helm-hide-minibuffer ()
;;     (when (with-helm-buffer helm-echo-input-in-header-line)
;;       (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;         (overlay-put ov 'window (selected-window))
;;         (overlay-put ov 'face
;;                      (let ((bg-color (face-background 'default nil)))
;;                        `(:background ,bg-color :foreground ,bg-color)))
;;         (setq-local cursor-type nil))))
;;   (add-hook 'helm-minibuffer-set-up-hook 'daedreth/helm-hide-minibuffer)
;;   (setq helm-autoresize-max-height 0
;;         helm-autoresize-min-height 40
;;         helm-M-x-fuzzy-match t
;;         helm-buffers-fuzzy-matching t
;;         helm-recentf-fuzzy-match t
;;         helm-semantic-fuzzy-match t
;;         helm-imenu-fuzzy-match t
;;         helm-split-window-in-side-p nil
;;         helm-move-to-line-cycle-in-source nil
;;         helm-ff-search-library-in-sexp t
;;         helm-scroll-amount 8
;;         helm-echo-input-in-header-line t)
;;   :init
;;   (helm-mode 1))

;; (require 'helm-config)
;; (helm-autoresize-mode 1)
