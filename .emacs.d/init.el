;;
;; Tim Williams' emacs configuration
;; Last Updated: 12 Feb 2015

;; disable parts of UI early in startup to avoid momentary display
(tool-bar-mode -1)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(tooltip-mode 0)

(set-default-font "Consolas 12")

;; Emacs 24 themes folder - NOT FOR LAPTOP
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'zenburn t)
;; (load-theme 'solarized-dark t)

;; inverse video settings
;; (setq inverse-video t)
;; (setq mode-line-inverse-video nil)

;; TODO this is needed for emacs 23 in Ubuntu 12.04
;; (x-handle-reverse-video (selected-frame) '((reverse . t)))

;; transparency with xcompmgr
;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
(set-face-foreground 'mode-line "#a6e22e")
(set-face-background 'mode-line "#1a2022")
(set-face-background 'mode-line-inactive "black")
(set-face-foreground 'mode-line-inactive "white")

;; disable horrible looking 3D mode-line
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; undo/redo window configurations
(winner-mode 1)

;; consistent with gnome terminals
;; (set-scroll-bar-mode 'right)

;; X11 Copy & Paste to/from Emacs
(setq x-select-enable-clipboard t)

;; Prefer UTF-8 encoding
(prefer-coding-system 'utf-8)

;; sh-mode should be saved with \n
(add-hook
 'sh-mode-hook
 (lambda ()
    (setq buffer-file-coding-system 'utf-8-unix)
    ))

;; Session management via Emacs Desktop
;; NB. file-related behavior
(require 'desktop)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(desktop-save-mode 1)
;; (defun my-desktop-save ()
;;    (interactive)
;;    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
;;    (if (eq (desktop-owner) (emacs-pid))
;;        (desktop-save desktop-dirname)))
;; (add-hook 'auto-save-hook 'my-desktop-save)

;; Various toys.
(mouse-wheel-mode t)  		; Make the mouse wheel work.
(blink-cursor-mode nil)			; Don't blink cursor.
(display-time)				; Always display the time.
(column-number-mode t)			; Column numbers on.
(which-function-mode t)                 ; show in mode line
(set-face-foreground 'which-func "yellow")

(show-paren-mode t)			; Automatically highlight parens.
(setq show-paren-delay 0)
;; (setq show-paren-style 'expression)     ; alternatives are 'parenthesis' and 'mixed'

(global-font-lock-mode t)		; Syntax highlighting by default
;; (iswitchb-mode 1)                    ; Use iswitchb. USE IDO INSTEAD

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
(setq mouse-autoselect-window t)

;; Use shift-arrow to jump between buffers
;; (windmove-default-keybindings)

;; F11 full screen - when not using xmonad
;; (defun switch-full-screen ()
;;       (interactive)
;;       (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
;; (global-set-key [f11] 'switch-full-screen)

(add-hook 'text-mode-hook 'visual-line-mode)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; show function context in modeline
;; (which-function 1)

;; C-x C-j for open dired at current
(require 'dired-x)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'ansi-color)
(require 'recentf)
(require 'linum)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; whitespace handling
(setq-default show-trailing-whitespace t) ; See the trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;; Load up ELPA, the package manager
;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("melpa" . "http://melpa.org/packages/")
                  ("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
                  ("elpa" . "http://tromey.com/elpa/")))
(add-to-list 'package-archives source t))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp" load-path)
(require 'markerpen)
(require 'tim-defaults)
(require 'tim-shell)
(require 'tim-utils)
(require 'tim-bindings)
(require 'tim-haskell)

(delete-selection-mode 1)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Set this to whatever browser you use
(setq browse-url-browser-function 'browse-default-windows-browser)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

;; ido-mode is awesome but sometimes it gets in the way. To temporarily disable it,
;; press C-f while the prompt is open. You can also press C-j while it's still enabled to
;; force the creation of the name.
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;;(add-hook 'text-mode-hook 'turn-on-flyspell)


;; eDiff
;;;;;;;;
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

;; Default to unified diffs
(setq diff-switches "-u -w")

;; upgraded flyspell
(add-to-list 'load-path "~/.emacs.d/opt/flyspell-1_7p.el" load-path)
(setq table-disable-incompatibility-warning t)

;; browse kill ring
(require 'browse-kill-ring)

;; tree undo
;; (setq load-path (cons "~/.emacs.d/opt/undo-tree" load-path))
;; (require 'undo-tree)
;;(global-undo-tree-mode)

;; markdown
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
      "Major mode for editing Markdown files" t)
     (setq auto-mode-alist
        (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; pandoc
(add-hook 'markdown-mode-hook 'turn-on-pandoc)

;; Magit
(require 'magit)

;; orgmode.org
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-mobile-files (quote ("notes.org")))
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; Graphviz
;; (load-file "~/.emacs.d/opt/graphviz-dot-mode.el")

;; paredit hooks
;;(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;;(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;;(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

;; nxml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode) auto-mode-alist))

;; Ben's suggestion to stop too much output killing emacs
(setq comint-buffer-maximum-size 9999)

; Agda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agda mode code which should run before the first Agda file is
;; loaded
;; (add-to-list 'load-path "~/.cabal/share/Agda-2.3.2/emacs-mode" load-path)

;; (autoload 'agda2-mode "agda2-mode"
;;   "Major mode for editing Agda files (version ≥ 2)." t)
;; (add-to-list 'auto-mode-alist '("\\.l?agda\\'" . agda2-mode))
;; (modify-coding-system-alist 'file "\\.l?agda\\'" 'utf-8)
;; (provide 'agda2)
;; (put 'dired-find-alternate-file 'disabled nil)
;; (put 'scroll-left 'disabled nil)
;; (put 'downcase-region 'disabled nil)

;; fsharp
;; (add-to-list 'load-path "~/.emacs.d/fsharp-mode/")
;; (autoload 'fsharp-mode "fsharp-mode"     "Major mode for editing F# code." t)
;; (add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))

;; Scala - DOESN'T WORK (JAN 2015)
;; (setenv "JDK_HOME" "/usr/lib/jvm/jdk1.8.0_25")
;; (setenv "JAVA_HOME" "/usr/lib/jvm/jdk1.8.0_25")
;; (setenv "PATH" (concat "/home/tim/opt/scala-2.11.5/bin:" (getenv "PATH")))
;; (setenv "PATH" (concat "/home/tim/opt/sbt/bin:" (getenv "PATH")))
;; (require 'scala-mode2)
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
