;;; init.el -*- lexical-binding: t; -*-

;; Tim Williams' emacs configuration
;; Last Updated: January 2018

;; disable parts of UI early in startup to avoid momentary display
(tool-bar-mode -1)
(menu-bar-mode -99) ;; disable the menu bar but still have it available on a popup menu (C-<mouse right>)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(tooltip-mode 0)

(when (string-equal system-type "windows-nt")
  (setq exec-path
        '("C:/cygwin64/bin"
          "C:/Program Files (x86)/Emacs/bin/"
          )))

;;;;;;;;;;;;;;;;; Load up ELPA, the package manager
;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(require 'package)
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                       ;;  ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ))

(package-initialize)
;;;;;;;;;;;;;;;;;;;;

;; set in Xresources
;;(set-default-font "Source Code Pro 11")
(set-default-font "Consolas 18")
(set-face-attribute 'default nil :font "Consolas 18" )
(set-frame-font "Consolas 18" nil t)

(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;; load this early
;; (use-package apropospriate-theme
;;   :ensure t
;;   :config
;;   (load-theme 'apropospriate-dark t)
;;   ;; apropospriate theme -- I want to better see the border of an inactive window
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :box '(:line-width 1 :color "#303030" :style nil)
;;                       :background "#474747" :foreground "#9E9E9E" :height 0.95)
;;   (set-face-attribute 'mode-line nil
;;                       :box '(:line-width 1 :color "#303030" :style nil))
;;   )

;; load this early
(use-package solarized-theme
  :ensure t
  :pin melpa-stable
  :demand
  :bind
  ;; toggle light/dark theme
  ("C-c t" . toggle-dark-light-theme)

  :config
  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)

  ;; support toggle light/dark
  (defun toggle-dark-light-theme ()
    (interactive)
    (if (eq active-theme 'solarized-light)
        (setq active-theme 'solarized-dark)
      (setq active-theme 'solarized-light))
    (load-theme active-theme))

  (setq active-theme 'solarized-light)
  (load-theme 'solarized-light t)
  )

;; undo/redo window configurations
(winner-mode 1)

;; (set-scroll-bar-mode 'right)

;; X11 Copy & Paste to/from Emacs
(setq x-select-enable-clipboard t)

;; Prefer UTF-8 encoding
(prefer-coding-system 'utf-8)

;; open-buffers read-only by default, C-x C-q to toggle read/write.
;; (add-hook 'find-file-hook
;;   '(lambda ()
;;      (when (and (buffer-file-name)
;;         (file-exists-p (buffer-file-name))
;;         (file-writable-p (buffer-file-name)))
;;        (message "Toggle to read-only for existing file")
;;        (toggle-read-only 1))))

;; sh-mode should be saved with \n
(add-hook
 'sh-mode-hook
 (lambda ()
    (setq buffer-file-coding-system 'utf-8-unix)
    ))

;; mouse settings
(mouse-wheel-mode t)  	                            ;; Make the mouse wheel work.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse

(blink-cursor-mode nil) ;; Don't blink cursor.
(display-time)		;; Always display the time.
(column-number-mode t)	;; Column numbers on.
(which-function-mode t) ;; show in mode line
(show-paren-mode t)	;; Automatically highlight parens.
(setq show-paren-delay 0)
(setq show-paren-style 'expression) ; alternatives are 'parenthesis' and 'mixed'

(global-font-lock-mode t) ; Syntax highlighting by default

;; Emacs will not automatically add new lines
(setq next-line-add-newlines nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Scroll down with the cursor,move down the buffer one
;; line at a time, instead of in larger amounts.
(setq scroll-step 1)

;; do not make backup files
(setq make-backup-files nil)

;; It's annoying to have your .emacs file modified by Emacs' "custom"
;; library, especially if you check in your .emacs to a source code
;; control system such as git (which you should do) and synchronize it
;; on multiple machines. Keep those customizations in a separate file:
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Buffer focus follows mouse
;; * This isn't really needed and can cause accidental switch, esp. on laptops.
(setq mouse-autoselect-window 't)

;; visual-line-mode allows reflow of text according to buffer width
;; and behaves as if there were breaks
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(add-hook 'text-mode-hook 'visual-line-mode)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; whitespace handling
(setq-default show-trailing-whitespace t) ; See the trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'diff-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'sr-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; inserting text while the mark is active causes the selected text to be deleted first.
(delete-selection-mode 1)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Set this to whatever browser you use
;; At my place of work, I am forced to use Chrome
;;(setq browse-url-browser-function 'browse-url-generic
;;      browse-url-generic-program "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe")

;; Transparently open compressed files
(auto-compression-mode t)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;; Filter out object files
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".prof_o")
(setq ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.o$" "\\.hi" "\\.exe"))

;; local packages
(add-to-list 'load-path "~/.emacs.d/lisp" load-path)
(require 'markerpen)
(require 'tim-utils)
(require 'tim-haskell) ;; a hacked-up version for my Win10 box

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup key bindings unrelated to any packages

;; NOTE: These bindings go in override-global-map and will take
;; precedence over any other mode maps.
(bind-keys*
 ("M-o"     . other-window)
 ("C-x o"   . window-swap-states)
 ("M-O"     . other-frame)
 ("C-x TAB" . indent-rigidly) ;; indent-rigidly
 ("C-x \\"  . align-regexp) ;; Align your code in a pretty way.
 ("M-/"     . dabbrev-expand) ;; Completion - was hippie-expand
 ("M-c"     . toggle-char-case) ;; set this key to a similar but saner idea
 ("C-c C-g" . vc-git-grep2) ;; seems as fast as "ag"
 ("M-X"     . smex-major-mode-commands)
 ("C-c r"   . revert-buffer)
 ("C-<f5>"  . display-line-numbers-mode) ;; add line-numbers to buffer
 ("C-h a"   . apropos) ;; Help should search more than just commands
 ("C-c b"   . copy-file-name-to-clipboard)) ;; copy file name to clipboard

;; Font size
(bind-keys*
 ("C-+" . text-scale-increase)
 ("C--" . text-scale-decrease))

;; Use regex searches by default.
(bind-keys*
  ("C-s"   . isearch-forward-regexp)
  ("C-r"   . isearch-backward-regexp)
  ("C-M-s" . isearch-forward)
  ("C-M-r" . isearch-backward))

;; Window re-sizing - that is friendly with org-mode
(bind-keys*
  ("C-}" . enlarge-window-horizontally)
  ("C-{" . shrink-window-horizontally)
  ("C-^" . enlarge-window))

;; Start a regular shell
;; C-x m orginallytaken by 'compose-mail'
(bind-key* "C-x m" 'eshell)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-c C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; this supports both scrolling horizontally and stops the
;; denial-of-service logging when vertically scrolling with my
;; trackball.
(defun tim/scroll-right ()
  (interactive)
  (scroll-right 1))
(defun tim/scroll-left ()
  (interactive)
  (scroll-left 1))
(global-set-key [wheel-left]  #'tim/scroll-left)
(global-set-key [wheel-right] #'tim/scroll-right)



;; eDiff
;;;;;;;;

;;(setq ediff-diff-program "C:/Users/timwi/AppData/Roaming/local/bin/pdiff.exe")

;; Do everything in one frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Split windows horizontally rather than vertically.
(setq ediff-split-window-function 'split-window-horizontally)

;; restore window configuration after ediff
(add-hook 'ediff-load-hook
          (lambda ()
            (add-hook 'ediff-before-setup-hook
                      (lambda ()
                        (setq ediff-saved-window-configuration (current-window-configuration))))
            (let ((restore-window-configuration
                   (lambda ()
                     (set-window-configuration ediff-saved-window-configuration))))
              (add-hook 'ediff-quit-hook restore-window-configuration 'append)
              (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

;; Associate modes with file extensions
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
    (add-to-list 'grep-find-ignored-files "target")
    (add-to-list 'grep-find-ignored-files "*.class")))

;; stop grep buffer being filled with grep command line!
(add-hook 'grep-mode-hook
          (lambda () (setq truncate-lines t)))

;; Default to unified diffs
(setq diff-switches "-u -w")

;; Ben's suggestion to stop too much output killing emacs
(setq comint-buffer-maximum-size 9999)

;; nxml
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package flyspell
  :ensure t
  :pin melpa-stable
  :config
  (setq table-disable-incompatibility-warning t)
  (setq ispell-dictionary "british")
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  )

;; C-x C-j for open dired at current
(use-package dired-x
  :config
  ;; (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")) ;; omit dotfiles
  (setq
   dired-isearch-filenames 'dwim
   dired-dwim-target t
   dired-omit-extension '(".hi" ".p_hi" ".o" ".a")
   dired-listing-switches "-alh"
   )
  (add-hook
   'dired-mode-hook
   (lambda ()
     (dired-omit-mode 1)
     (define-key dired-mode-map "z" 'dired-zip-files)
     (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
     (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
   ))
  )

;; (use-package w32-browser
;;   :ensure t
;;   :pin melpa-stable
;;   :config
;;     (add-hook
;;    'dired-mode-hook
;;    (lambda ()
;;      (define-key dired-mode-map (kbd "M-RET") 'dired-w32-browser) ;; to open files with windows
;;      (define-key dired-mode-map (kbd "W") 'dired-w32explore) ;; to open directories in Win explorer
;; ;   (define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)
;;    ))
;;   )

(use-package smex
  :ensure t
  :pin melpa-stable
  :init (smex-initialize))
(use-package cl)
(use-package saveplace)
(use-package ffap)
(use-package ansi-color)

;; Save a list of recent files visited.
(use-package recentf
  :config
  (setq recentf-save-file "~/.emacs.d/recentf"
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )

(use-package drag-stuff
  :ensure t
  :pin melpa-stable
  :init (drag-stuff-global-mode 1)
  :bind
  ("M-P" . drag-stuff-up)
  ("M-N" . drag-stuff-down)
  )

(use-package paredit
  :ensure t
  :pin melpa-stable
  :bind
  ("C-M-f" . paredit-forward)
  ("C-M-b" . paredit-backward)
  ("C-M-u" . paredit-backward-up)
  ("C-M-n" . paredit-forward-up)
  ("C-M-d" . paredit-forward-down)
  ("M-S"   . paredit-splice-sexp)
  ("M-R"   . paredit-raise-sexp)
  ("M-("   . paredit-wrap-round)
  ("M-["   . paredit-wrap-square)
  ("M-{"   . paredit-wrap-curly)
  ("M-\""  . paredit-meta-doublequote)
  )

;; display available keys after 1 second delay
(use-package which-key
  :ensure t
  :pin melpa-stable
  :config (which-key-mode)
  )

(use-package ibuffer
  :config (setq ibuffer-expert t)
  :bind ("C-x C-b" . ibuffer))

;; Ivy
;;;;;;;;
(use-package ivy
  :ensure t
  :pin melpa-stable
  :config
  (ivy-mode 1)
  (setq ivy-height 10
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-display-style 'fancy
        ivy-count-format "%d/%d ")
  :bind
  ("C-x b" . ivy-switch-buffer))

(use-package swiper
  :ensure t
  :pin melpa-stable)

(use-package counsel
  :ensure t
  :pin melpa-stable
  :bind
  ("M-x"     . counsel-M-x)
  ("M-y"     . counsel-yank-pop)
  ("C-x C-f" . counsel-find-file)

  ;; NOTE "C-c C-o" is bound to 'ivy-occur during the prompt for these
  ("C-c s"   . counsel-grep-or-swiper)
  ;; ("C-c g"   . counsel-git-grep) ; use vc-git-grep2 or counsel-projectile-grep instead
  ("C-c u"   . swiper-all)

  ("C-x f"   . counsel-recentf)

  ("C-c f"   . counsel-describe-function)
  ("C-c v"   . counsel-describe-variable)
  ("C-c k"   . counsel-rg)
  ("C-c i"   . counsel-imenu)
  :config
  (setq completion-in-region-function 'ivy-completion-in-region)
  )

(use-package counsel-etags
  :ensure t
  :pin melpa-stable)

;; NOTE "C-c p s g" for counsel-projectile-grep
(use-package counsel-projectile
  :ensure t
  :pin melpa-stable)

(use-package yasnippet
  :ensure t
  :pin melpa-stable
  :config
  (yas-global-mode 1))

(use-package ripgrep
  :ensure t)

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind
  ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  )

;; completion pop-ups
(use-package company
  :ensure t
  :pin melpa-stable
  :defer t ;; company can be slow to initialise
  :config
  (setq company-dabbrev-downcase nil) ;; switch-off a crazy default
  (global-company-mode)
  :bind
  ("M-?" . company-complete))

;; NOTE: currently only in Melpa
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; NOTE: potentially issues with using from network shares
(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (projectile-global-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :init
  ;; (setq projectile-require-project-root nil)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-indexing-method 'alien)
  (setq projectile-file-exists-local-cache-expire (* 5 60)) ;; workaround freezes on slow SMB shares
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode.org

(use-package org
  :ensure t
  :pin melpa-stable
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("M-p"   . backward-paragraph) ;; paragraph navigation
  ("M-n"   . forward-paragraph)

  :config

  (setq org-todo-keywords '((type "TODO(t@/!)" "IN-PROGRESS(i@/!)" "WAITING(w@/!)" "|" "DONE(d@/!)" "CANCELLED(c@/!)")))

  (setq org-use-fast-todo-selection t)
  (setq org-log-done 'time)

  ;; (add-hook 'org-mode-hook 'turn-off-auto-fill)
  ;; (add-hook 'org-mode-hook 'toggle-truncate-lines)
  (add-hook 'org-mode-hook 'visual-line-mode)

  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

  (setq org-default-notes-file "~/org/notes.org")

  (org-babel-do-load-languages
   'org-babel-load-languages '((dot . t) (plantuml . t) (haskell . t)))

    ;; stop annoying confirmations at the expense of security
  (setq org-confirm-babel-evaluate nil)

  (setq org-plantuml-jar-path
        (expand-file-name "~/org/plantuml.jar"))

  (setq org-cycle-separator-lines 1)

  (setq org-refile-use-outline-path nil)     ; Show full paths for refiling
  (setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

  (setq org-agenda-files '("~/org"))

  (setq org-capture-templates
     '(("t" "Todo" entry (file+headline "~/org/notes.org" "Inbox")
            "* TODO %?\n  %i\n")
       ("j" "Journal" entry (file+datetree "~/org/notes.org")
            "* %?\n%U\n  %i\n")))
  )

;; markdown
(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.\\(md\\|markdown\\)\\'"
  :bind
  ;; Blocks in markdown-mode are code blocks, blockquotes, list
  ;; items, headings, horizontal rules, or plain text paragraphs
  ;; separated by whitespace.
  (:map markdown-mode-map
        ("M-p"     . markdown-forward-block)
        ("M-n"     . markdown-backward-block))
  :config (progn
            (use-package pandoc-mode :ensure t)
            (add-hook 'markdown-mode-hook
                      (lambda ()
                        (conditionally-turn-on-pandoc)
                        (visual-line-mode)
                        (flyspell-mode)
                        (pandoc-mode)))))

;; PDF-Tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package org-pdfview
  :ensure t)

;; Session management via Emacs Desktop
;; NB. file-related behavior
(use-package desktop
  :config
  (setq desktop-path '("~/.emacs.d/"))
  (setq desktop-dirname "~/.emacs.d/")
  (setq desktop-base-file-name "emacs-desktop")
  (desktop-save-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for emacs client
(server-start)
