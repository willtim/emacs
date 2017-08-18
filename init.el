;;
;; Tim Williams' emacs configuration
;; Last Updated: 1st September 2016

;; disable parts of UI early in startup to avoid momentary display
(tool-bar-mode -1)
(menu-bar-mode -99) ;; disable the menu bar but still have it available on a popup menu (C-<mouse right>)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(tooltip-mode 0)

;;;;;;;;;;;;;;;;; Load up ELPA, the package manager
;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(require 'package)
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;

;; set in Xresources
;;(set-default-font "Source Code Pro 11")
(set-default-font "Consolas 15")
(set-face-attribute 'default nil :font "Consolas 15" )
(set-frame-font "Consolas 15" nil t)

(require 'apropospriate)
(load-theme 'apropospriate-dark t)
;; or
;;(load-theme 'apropospriate-light t)

;; apropospriate theme tweak for TN panels at work.
;; I want to better see the border of an inactive window
(set-face-attribute 'mode-line-inactive nil
                    :box '(:line-width 4 :color "#303030" :style nil)
                    :background "#474747" :foreground "#9E9E9E" :height 0.95)

;; (load-theme 'zenburn t)
;; (load-theme 'solarized-dark t)

;; inverse video settings
;; (setq inverse-video t)
;; (setq mode-line-inverse-video nil)

;; TODO this is needed for emacs 23 in Ubuntu 12.04
;; (x-handle-reverse-video (selected-frame) '((reverse . t)))

;; transparency with xcompmgr/compton
;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;; (set-frame-parameter (selected-frame) 'alpha '(85 85))
;; (add-to-list 'default-frame-alist '(alpha 85 85))
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;; clean-up modeline - not needed for apropospriate theme
;; (set-face-foreground 'mode-line "#a6e22e")
;; (set-face-background 'mode-line "#1a2022")
;; (set-face-background 'mode-line-inactive "black")
;; (set-face-foreground 'mode-line-inactive "white")
;; disable horrible looking 3D mode-line
;; (set-face-attribute 'mode-line nil :box nil)
;; (set-face-attribute 'mode-line-inactive nil :box nil)

;; undo/redo window configurations
(winner-mode 1)

;; consistent with gnome terminals
;; (set-scroll-bar-mode 'right)

;; X11 Copy & Paste to/from Emacs
;; (setq x-select-enable-clipboard t)

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


;; mouse settings
(mouse-wheel-mode t)  	                            ;; Make the mouse wheel work.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse

;; Various toys.
(blink-cursor-mode nil) ;; Don't blink cursor.
(display-time)		;; Always display the time.
(column-number-mode t)	;; Column numbers on.
(which-function-mode t) ;; show in mode line
(set-face-foreground 'which-func "yellow")

(show-paren-mode t)	;; Automatically highlight parens.
(setq show-paren-delay 0)
;; (setq show-paren-style 'expression)	   ; alternatives are 'parenthesis' and 'mixed'

(global-font-lock-mode t)               ; Syntax highlighting by default
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
;; * This isn't really needed and can cause accidental switch, esp. on laptops.
(setq mouse-autoselect-window nil)

;; Use shift-arrow to jump between buffers
;; (windmove-default-keybindings)

;; F11 full screen - when not using xmonad
;; (defun switch-full-screen ()
;;       (interactive)
;;       (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
;; (global-set-key [f11] 'switch-full-screen)

;; visual-line-mode allows reflow of text according to buffer width
;; and behaves as if there were breaks
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(add-hook 'text-mode-hook 'visual-line-mode)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Automatically introduces closing parenthesis, brackets, braces, etc.
(electric-pair-mode 0) ;; turn-off, use paredit "M-(" etc instead.

;; C-x C-j for open dired at current
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")) ;; omit dotfiles

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session
(require 'smex)
(smex-initialize)
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
(add-hook 'diff-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'sr-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; local packages
(add-to-list 'load-path "~/.emacs.d/lisp" load-path)
(require 'markerpen)
(require 'tim-functions)
(require 'tim-defaults)
(require 'tim-shell)
(require 'tim-utils)
(require 'tim-bindings)
(require 'tim-haskell)

;; inserting text while the mark is active causes the selected text to be deleted first.
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
(setq recentf-max-saved-items 50)

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

;; Filter out object files
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".prof_o")
(setq ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.o$" "\\.hi" "\\.exe"))
(setq dired-omit-extension '(".hi" ".p_hi" ".o" ".a"))

(setq
  dired-isearch-filenames 'dwim
  dired-dwim-target t
  dired-omit-extension '(".hi" ".p_hi" ".o" ".a")
  dired-listing-switches "-alh"
)

;; useful windows bindings
(require 'w32-browser)

(add-hook
 'dired-mode-hook
 (lambda ()
   (dired-omit-mode 1) ;; see above
   (define-key dired-mode-map "z" 'dired-zip-files)
   (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
   (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
   (define-key dired-mode-map (kbd "M-RET") 'dired-w32-browser) ;; to open files with windows
   (define-key dired-mode-map (kbd "W") 'dired-w32explore) ;; to open directories in Win explorer
   (define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)
   ))

;; completion pop-ups
(require 'company)
(company-mode-on)

;; display available keys after 1 second delay
(which-key-mode)

;; Ivy
;;;;;;;;
(require 'ivy)
;; (ivy-mode 1) ; breaks rgrep
(setq ivy-height 10)
(setq ivy-use-virtual-buffers t)
(setq ivy-display-style 'fancy)

(require 'swiper)
(require 'counsel)
;; swiper, swiper-multi and swiper-all.
;;  counsel-M-x, counsel-git-grep, and counsel-grep-or-swiper.

(setq completion-in-region-function 'ivy-completion-in-region)
(setq magit-completing-read-function 'ivy-completing-read)


;; eDiff
;;;;;;;;

(setq ediff-diff-program "C:/Users/timwi/AppData/Roaming/local/bin/pdiff.exe")

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

;; upgraded flyspell
(add-to-list 'load-path "~/.emacs.d/opt/flyspell-1_7p.el" load-path)
(setq table-disable-incompatibility-warning t)

;; Sunrise
;;tweak faces for paths
(require 'sunrise-commander)
(set-face-attribute 'sr-active-path-face nil
                    :background "black")
(set-face-attribute 'sr-passive-path-face nil
                    :background "black")

;; This nasty hack of a package breaks things
;; (require 'openwith)
;; (openwith-mode t)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode.org

(require 'org-install)
(setq org-use-fast-todo-selection t)
(setq org-log-done 'time)

;; (add-hook 'org-mode-hook 'turn-off-auto-fill)
;; (add-hook 'org-mode-hook 'toggle-truncate-lines)
(add-hook 'org-mode-hook 'visual-line-mode)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-default-notes-file "~/org/notes.org")
(define-key global-map "\C-cc" 'org-capture)

;(setq org-mobile-files (quote ("notes.org")))
;(setq org-mobile-directory "~/Dropbox/MobileOrg")

(org-babel-do-load-languages
 'org-babel-load-languages '((dot . t) (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "~/org/plantuml.jar"))

(setq org-capture-templates
   '(("t" "Todo" entry (file+headline "~/Dropbox/MobileOrg/gtd.org" "In-Box")
          "* TODO %?\n  %i\n  %a")
     ("j" "Journal" entry (file+datetree "~/Dropbox/MobileOrg/journal.org")
          "* %?\nEntered: %U\n  %i\n  %a")))


(setq org-agenda-files (list "~/Dropbox/MobileOrg/gtd.org"
                             "~/Dropbox/MobileOrg/tim-calendar.org"
                             "~/Dropbox/MobileOrg/karen-calendar.org"
                             ))

(setq org-agenda-custom-commands
           '(("D" "Daily Action List"
      (
           (agenda "" ((org-agenda-ndays 1)
                       (org-agenda-sorting-strategy
                        (quote ((agenda time-up priority-down tag-up) )))
                       (org-deadline-warning-days 0)
                       ))))))

(require 'calfw)
(require 'calfw-org)

(require 'org-gcal)
(setq org-gcal-client-id "475236531230-d4cue8pvr2cqe2mibdqk6l21c3q0oeel.apps.googleusercontent.com"
      org-gcal-client-secret "oSbcQr0FlkzxjZeg269Zd1zN"
      org-gcal-file-alist '(("tim.philip.williams@gmail.com" .  "~/Dropbox/MobileOrg/tim-calendar.org")
                            ("karencornish@hotmail.co.uk"    .  "~/Dropbox/MobileOrg/karen-calendar.org")
                            ))
(setq browse-url-browser-function 'browse-url-default-windows-browser)

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
;; (put 'downcase-region 'disabled nil)

;; fsharp
;; (unless (package-installed-p 'fsharp-mode)
;;   (package-install 'fsharp-mode))
;; (require 'fsharp-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for emacs client
(server-start)

(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
