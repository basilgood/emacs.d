;;; eshell.el --- summary
;;; commentary:
;;; code:

(use-package f
  :ensure t
  :config
  (setq eshell-visual-commands
    '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))
  (setq eshell-visual-subcommands
    '(("git" "log" "l" "diff" "show"))))

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(setq eshell-cmpl-cycle-completions nil)

;;; eshell.el ends here
