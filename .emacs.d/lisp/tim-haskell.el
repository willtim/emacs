;;; tim-haskell.el --- haskell-mode customizations
 
;; Haskell mode 2.8+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(load "~/.emacs.d/opt/haskell-mode/haskell-site-file")
(require 'haskell-mode)
;; (require 'ghci-completion)
 
(add-hook 'haskell-mode-hook
          (lambda ()
             (turn-on-haskell-decl-scan)
             (turn-on-haskell-doc-mode)
             (turn-on-haskell-indent)
             (subword-mode 1)
             (linum-mode 1))) ;; 1-on, 0-off
 
(set-default 'haskell-literate-default 'tex)
 
(add-hook 'inferior-haskell-mode-hook
          (lambda ()
           ;;  (preamble-turn-on-comint-history)
;;            (turn-on-ghci-completion)
            (subword-mode 0))) ;; 1-on, 0-off
 
;; Haskell specific keybindings
(define-key haskell-mode-map (kbd "C-c h")
  (lambda ()
    (interactive)
    (let ((haskell-hoogle-command "hoogle"))
      (call-interactively 'haskell-hoogle))))
 
(define-key haskell-mode-map (kbd "C-c C-r") 'inferior-haskell-reload-file)
 
(define-key haskell-mode-map (kbd "C-c C-p") 'pointful-region)
 
;; etags -- TODO append to existing list instead?
;;(setq tags-table-list
;;           (cons (concat fpf-main "\\Build\\TAGS") '()))

(setq haskell-program-name "ghci")
 
;;(setq ghci-completion-ghc-pkg-additional-args
;;      (list "-f" fpf-package-conf))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 
;; no limit to preview sizes
(setq max-image-size nil)
 
;; useful?
(defun pointful-region ()
  (interactive)
  (let ((query (buffer-substring (mark) (point))))
    (message (shell-command-to-string (concat "pointful.exe \"" query "\"")))))

(provide 'tim-haskell)
;;; tim-haskell.el ends here
