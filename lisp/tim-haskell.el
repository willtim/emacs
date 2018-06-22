;;; tim-haskell.el --- haskell-mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'haskell-mode)

(require 'yasnippet)
(require 'haskell-snippets)
(require 'nix-sandbox)
(require 'flycheck-haskell)


(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
;; (require 'ghci-completion)

(add-hook 'haskell-mode-hook
          (lambda ()
            ;; (inf-haskell-mode)
            (turn-on-haskell-decl-scan)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indentation)
            (flycheck-mode)
            (yas-minor-mode)
            (subword-mode 0)
            (linum-mode 1))) ;; 1-on, 0-off

(set-default 'haskell-literate-default 'tex)

;; (add-hook 'inferior-haskell-mode-hook
;;           (lambda ()
;;            ;;  (preamble-turn-on-comint-history)
;;            ;;  (turn-on-ghci-completion)
;;             (subword-mode 0))) ;; 1-on, 0-off

;; Haskell specific keybindings
(define-key haskell-mode-map (kbd "C-c h")
  (lambda ()
    (interactive)
    (let ((haskell-hoogle-command "hoogle"))
      (call-interactively 'haskell-hoogle))))


;; (define-key haskell-mode-map (kbd "C-c C-r") 'inferior-haskell-reload-file)

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

(define-key haskell-mode-map [f12] 'haskell-navigate-imports)
(define-key haskell-mode-map (kbd "C-c C-p") 'pointful-region)
(define-key haskell-mode-map (kbd "C-c C-h C-t") 'haskell-process-toggle)

(define-key haskell-mode-map (kbd "M-g M-n") 'flycheck-next-error)     ;; override next-error
(define-key haskell-mode-map (kbd "M-g M-p") 'flycheck-previous-error) ;; override previous-error

(define-key haskell-mode-map (kbd "C-c f") 'haskell-mode-stylish-buffer)
(define-key haskell-mode-map (kbd "C-c M-.") 'haskell-mode-jump-to-def-or-tag)

;; for when one is in the repl
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; etags -- TODO append to existing list instead?
;;(setq tags-table-list
;;           (cons (concat fpf-main "\\Build\\TAGS") '()))

;; generate tags on save
;;(custom-set-variables
;;  '(haskell-tags-on-save t))

;; This is for inferior-haskell
;;(setq haskell-program-name "nix-shell -p ghcEnv --run ghci")

;; This was for haskell-interactive-mode
;;(setq haskell-process-args-ghci "...")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suggestions

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck

(setq flycheck-command-wrapper-function
          (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
      flycheck-executable-find
          (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A function to toggle the process type

;; (defvar haskell-process-use-ghci nil)
;; (setq haskell-process-type 'cabal-repl)
(setq haskell-process-type 'cabal-repl)

(setq haskell-process-wrapper-function
     '(lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

;; (defun haskell-process-toggle ()
;;   "Toggle GHCi process between cabal and ghci"
;;   (interactive)
;;   (if haskell-process-use-ghci
;;       (progn (setq haskell-process-type 'cabal-repl)
;;              (setq haskell-process-use-ghci nil)
;;              (message "Using cabal repl"))
;;     (progn (setq haskell-process-type 'ghci)
;;            (setq haskell-process-use-ghci t)
;;            (message "Using GHCi"))))

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
