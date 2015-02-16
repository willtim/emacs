(require 'shell)
;; (require 'tim-ansi-color)

;; Do not highlight any additional expressions in shell mode.
(setq shell-font-lock-keywords nil)

;; Use the custom color theme.
;; (add-hook 'shell-mode-hook 'shell-update-color-map)

;; Track the current working directory by watching the prompt.
(add-hook 'shell-mode-hook
          (lambda ()
            (dirtrack-mode 1)))

;; Use bash when requesting inferior shells (also remote ones).
(setq shell-file-name "zsh")
(setq explicit-shell-file-name "/bin/zsh")

;; The standard way to have multiple shells is to open a shell, rename
;; its buffer, open a new shell, rename its buffer etc.  This is
;; tedious.  The following function makes it easier.
(defun named-shell (name directory)
  "Open a named shell. NAME is the base name of the shell buffer,
and DIRECTORY is the directory to open the shell in."
  (interactive "MName: \nDDirectory: ")
  (switch-to-buffer (concat "*" name "*"))
  (cd directory)
  (shell (current-buffer)))

(provide 'tim-shell)
