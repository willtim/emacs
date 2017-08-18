;;; tim-defaults.el --- misc default customizations
 
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
 
(setq
 visible-bell t
 scroll-bar-mode nil
 fill-column 80
;; browse-url-mozilla-program "chrome"
 column-number-mode t
 haskell-program-name "ghci"
 indent-tabs-mode nil
 inhibit-startup-screen t
 longlines-show-hard-newlines t
 password-cache-expiry 0
 python-guess-indent nil
 scroll-step 1
 sgml-basic-offset 4
 show-paren-mode t
;; show-trailing-whitespace t
 size-indication-mode t
 c-syntactic-indentation t
 c-tab-always-indent t
 tab-always-indent t
 tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
 tab-width 4
 
 nxml-child-indent 4
 nxml-outline-child-indent 4
 nxml-slash-auto-complete-flag t
 
 fringe-mode (cons 4 0)
 echo-keystrokes 0.1
 font-lock-maximum-decoration t
 inhibit-startup-message t
 transient-mark-mode t
 color-theme-is-global t
 shift-select-mode nil
 mouse-yank-at-point t
 require-final-newline t
 truncate-partial-width-windows nil
 ffap-machine-p-known 'reject
 whitespace-style '(trailing lines space-before-tab
                                  face indentation space-after-tab)
 whitespace-line-column 100
 ediff-window-setup-function 'ediff-setup-windows-plain
 xterm-mouse-mode t
 save-place-file "~/.emacs.d/places"
 
 ;; Get sunrise/sunset right!
 calendar-latitude 51.5
 calendar-longitude -0.1
 calendar-location-name "London, UK"
 calendar-week-start-day 1
 
 compilation-scroll-output t
 
 ido-default-buffer-method (quote selected-window)
)
 
(provide 'tim-defaults)
;;; tim-defaults.el ends here

