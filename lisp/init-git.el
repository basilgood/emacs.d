;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(when (maybe-require-package 'git-timemachine)
  (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))



(when(maybe-require-package 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch))

(provide 'init-git)
;;; init-git.el ends here
