;;; tim-haskell.el --- haskell-mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'haskell-mode)

(require 'yasnippet)
(require 'haskell-snippets)
(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
;; (require 'ghci-completion)


;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; Install Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

(add-hook 'haskell-mode-hook
          (lambda ()
             (turn-on-haskell-decl-scan)
             (turn-on-haskell-doc-mode)
             (turn-on-haskell-indentation)
             (yas-minor-mode)
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

;;(define-key haskell-mode-map (kbd "C-c C-r") 'inferior-haskell-reload-file)
(define-key haskell-mode-map (kbd "C-c C-p") 'pointful-region)
(define-key haskell-mode-map (kbd "M-g M-n") 'flycheck-next-error)     ;; override next-error
(define-key haskell-mode-map (kbd "M-g M-p") 'flycheck-previous-error) ;; override previous-error

(define-key haskell-mode-map (kbd "C-c f") 'haskell-mode-stylish-buffer)

;;(setq haskell-process-type 'stack-ghci)
;;(setq haskell-process-path-ghci "stack")
;;(setq haskell-process-args-ghci "ghci")

;; etags -- TODO append to existing list instead?
;;(setq tags-table-list
;;           (cons (concat fpf-main "\\Build\\TAGS") '()))

;;(setq haskell-program-name "ghci")

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
