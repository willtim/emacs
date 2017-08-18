;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


(defun toggle-char-case ()
  "Toggle the case of the char under the cursor. Why is this not in Emacs!?"
  (interactive)
  (let ((case-fold-search nil))
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))))


;; This appears to always return results (unlike vc-git-grep), is case
;; insensitive and does not require a filename extension.
(defun vc-git-grep2 (regexp dir)
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep" nil nil 'grep-history)
             nil))
      (t (let* ((regexp (grep-read-regexp))
                (dir (read-directory-name "In directory: " nil default-directory t)))
           (list regexp dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (> 4 5)
          (if (string= command "git grep")
              (setq command nil))
        (setq dir (file-name-as-directory (expand-file-name dir)))
        (setq command
              (grep-expand-template "git grep -n -i -e <R>" regexp))
        (when command
          (if (equal current-prefix-arg '(4))
              (setq command
                    (read-from-minibuffer "Confirm: " command nil nil 'grep-history))
            (add-to-history 'grep-history command))))
      (when command
        (let ((default-directory dir)
              (compilation-environment '("PAGER=")))
          ;; Setting process-setup-function makes exit-message-function work
          ;; even when async processes aren't supported.
          (compilation-start command 'grep-mode))
        (if (eq next-error-last-buffer (current-buffer))
            (setq default-directory dir))))))

(provide 'tim-functions)
;;; tim-haskell.el ends here
