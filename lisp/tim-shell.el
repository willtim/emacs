 ;;; tim-shell.el --- cygwin support and shell customizations

;; much taken from:
;; http://www.cygwin.com/faq/faq-nochunks.html#faq.using.ntemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; needed to make NTEmacs understand cygwin ptys, otherwise job control
;; does not work and some things hang.
;; https://github.com/d5884/fakecygpty
(require 'fakecygpty)
(fakecygpty-activate)

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'cygwin-mount)

;;; Make Cygwin paths accessible
(cygwin-mount-activate)

;; Do not highlight any additional expressions in shell mode.
(setq shell-font-lock-keywords nil)

;; This assumes that Cygwin is installed in C:\cygwin64 (the
;; default) and that C:\cygwin64\bin is not already in your
;; Windows Path (it generally should not be).
(setq exec-path (cons "C:/cygwin64/bin" exec-path))
(setenv "PATH" (concat "C:\\cygwin64\\bin;" (getenv "PATH")))

(setenv "CYGWIN" "nodosfilewarning winsymlinks:lnk") ;; otherwise ediff doesn't work

;;; Use /dev/null, not NUL.
(setq null-device  "/dev/null")

(setq-default buffer-file-coding-system (coding-system-change-eol-conversion
                                         (default-value 'buffer-file-coding-system)
                                         'unix))

;;   LOGNAME and USER are expected in many Emacs packages
;;   Check these environment variables.

(if (and (null (getenv "USER"))
         ;; Windows includes variable USERNAME, which is copied to
         ;; LOGNAME and USER respectively.
         (getenv "USERNAME"))
    (setenv "USER" (getenv "USERNAME")))

(if (and (getenv "LOGNAME")
         ;;  Bash shell defines only LOGNAME
         (null (getenv "USER")))
    (setenv "USER" (getenv "LOGNAME")))

(if (and (getenv "USER")
         (null (getenv "LOGNAME")))
    (setenv "LOGNAME" (getenv "USER")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (A) M-x shell: This change M-x shell permanently
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Would call Windows command interpreter. Change it.

(setq explicit-shell-file-name "C:/cygwin64/bin/bash.exe")
(setq shell-file-name "bash")
(setq explicit-bash.exe-args '("--noediting" "-i"))
(setenv "SHELL" shell-file-name)


;; Remove C-m (^M) characters that appear in output
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;; dirtrack mode
;;;;;;;;;;;;;;;;
;; Track the current working directory in shell mode by watching the prompt.
(require 'dirtrack)

(add-hook 'shell-mode-hook
          (lambda ()
            (dirtrack-mode 1)
            (setq dirtrack-list '("^|PrOmPt|\\([^|]*\\)|" 1 nil))
            (shell-dirtrack-mode nil)
            (add-hook 'comint-preoutput-filter-functions
                      'dirtrack nil t)
            (add-hook 'comint-preoutput-filter-functions
                      'dirtrack-filter-out-pwd-prompt t t)))


;; Now strip the goofy strings from the prompt before it gets into
;; the shell buffer.
(defun dirtrack-filter-out-pwd-prompt (string)
  "dirtrack-mode doesn't remove the PWD match from the prompt.  This does."
  ;; TODO: support dirtrack-mode's multiline regexp.
  (if (and (stringp string) (string-match (first dirtrack-list) string))
      (replace-match "" t t string 0)
    string))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tim-shell)
;;; tim-shell.el ends here
