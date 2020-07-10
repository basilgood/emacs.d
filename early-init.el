;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst my/start-time (current-time))

(defmacro mt (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(add-hook 'window-setup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  gc-cons-threshold 800000
                  gc-cons-percentage 0.1)
	    (garbage-collect)
	    (message "Load time %.06f" (float-time (time-since my/start-time))))
	  t)

(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)

(load
  (expand-file-name "init.el" user-emacs-directory) nil 'nomessage 'nosuffix)

(provide 'early-init)
;;; early-init.el ends here
