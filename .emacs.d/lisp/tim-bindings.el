;;; tim-bindings.el --- Set up some handy key bindings

;; set this key to a similar but saner idea
(global-set-key (kbd "M-c") 'toggle-char-case)

;; seems as fast as "ag"
(global-set-key (kbd "C-c C-g") 'vc-git-grep2)

(global-set-key (kbd "M-y") 'counsel-yank-pop)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; NOTE "C-c C-o" is bound to 'ivy-occur during the prompt for these
(global-set-key (kbd "C-c s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-c g") 'counsel-git-grep)
(global-set-key (kbd "C-c u") 'swiper-all)
;; counsel-ag ?

;; (global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "M-o")   'switch-window)
(global-set-key (kbd "C-x o") 'switch-window-then-swap-buffer)
;; (global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)
;; (global-set-key "\M-`" 'next-multiframe-window)

;; Ido/counsel over imenu - jump to a definition in the current file.
(global-set-key (kbd "C-c i") 'counsel-imenu)
;; (global-set-key (kbd "C-c i") 'ido-imenu)
;; (global-set-key (kbd "C-c i") 'ido-goto-symbol)

;; indent-rigidly
(global-set-key (kbd "C-x TAB") 'indent-rigidly)

(require 'drag-stuff)
(global-set-key (kbd "M-p") #'drag-stuff-up)
(global-set-key (kbd "M-n") #'drag-stuff-down)

(require 'paredit)
(global-set-key (kbd "C-M-u") #'paredit-backward-up)
(global-set-key (kbd "C-M-n") #'paredit-forward-up)
;; This one's surpisingly useful for writing prose.
(global-set-key "\M-S"
  #'paredit-splice-sexp-killing-backward)
(global-set-key "\M-R" #'paredit-raise-sexp)
(global-set-key "\M-(" #'paredit-wrap-round)
(global-set-key "\M-[" #'paredit-wrap-square)
(global-set-key "\M-{" #'paredit-wrap-curly)

;; Sunrise commander
(global-set-key (kbd "C-c x") 'sunrise)
(global-set-key (kbd "C-c X") 'sunrise-cd)

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Completion - was hippie-expand
(global-set-key (kbd "M-/") 'dabbrev-expand)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; add line-numbers to buffer
(global-set-key (kbd "C-<f5>") 'linum-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; File finding
;; (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;; (global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
;; (global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x f") 'ivy-recentf)
;;(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
;; (global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)

;; Electronic Buffer List instead of BufferMenu, SPC to select and RET to bury the buffer
;;(global-set-key "\C-x\C-b" 'electric-buffer-list)
;; Use the new ibuffer instead of the above
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Window re-sizing - that is friendly with org-mode
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "C-^") 'enlarge-window)

;; Company mode
(global-set-key (kbd "M-?") 'company-complete)

;; Start a regular shell
;; C-x m orginallytaken by 'compose-mail'
(global-set-key (kbd "C-x m") 'named-shell)

;; If you want to be able to M-x without meta (phones, etc)
;; (global-set-key (kbd "C-x m") 'execute-extended-command)

;; Fetch the contents at a URL, display it raw.
;; (global-set-key (kbd "C-x C-u") 'view-url)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; For debugging Emacs modes
(global-set-key (kbd "C-c p") 'message-point)

;; Needs bindings I think
(global-set-key (kbd "C-x g") 'magit-status)


;; This is a little hacky since VC doesn't support git add internally
(eval-after-load 'vc
  (define-key vc-prefix-map "i" '(lambda () (interactive)
                                   (if (not (eq 'Git (vc-backend buffer-file-name)))
                                       (vc-register)
                                     (shell-command (format "git add %s" buffer-file-name))
                                     (message "Staged changes.")))))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-c C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; copy file name to clipboard
(global-set-key (kbd "C-c b") 'copy-file-name-to-clipboard)

(provide 'tim-bindings)
;;; tim-bindings.el ends here
