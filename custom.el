(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-default-windows-browser))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(cygwin-mount-cygwin-bin-directory "c:/cygwin64/bin")
 '(cygwin-mount-table t)
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoctor asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint d-dmd dockerfile-hadolint elixir-dogma emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-dante haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-jscs javascript-standard json-jsonlint json-python-json less less-stylelint lua-luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing protobuf-protoc pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint markdown-mdl nix rst-sphinx rst ruby-rubocop ruby-rubylint ruby ruby-jruby rust-cargo rust scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tex-chktex tex-lacheck texinfo typescript-tslint verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-pos-tip-timeout 60)
 '(grep-find-template
   "unset  GREP_OPTIONS; /bin/find . <X> -type f <F> -exec /usr/bin/grep <C> --regexp=<R> --with-filename --line-number --color=always {} +")
 '(haskell-indent-after-keywords
   (quote
    (("where" 2 0)
     ("of" 4)
     ("do" 4)
     ("mdo" 4)
     ("rec" 4)
     ("in" 4 0)
     ("{" 4)
     "if" "then" "else" "let")))
 '(haskell-indent-spaces 4)
 '(haskell-indentation-ifte-offset 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-indentation-starter-offset 4)
 '(haskell-mode-stylish-haskell-path "c:/workspace/ext/bin/stylish-haskell")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-tags-on-save t)
 '(ispell-dictionary "british")
 '(markdown-fontify-code-blocks-natively t)
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-confirm-babel-evaluate nil)
 '(org-deadline-warning-days 14)
 '(org-gcal-down-days 90)
 '(package-selected-packages
   (quote
    (org-capture org-install dired-x dired-subtree scala-mode flycheck-haskell haskell-mode nix-sandbox company-nixos-options nixos-options flycheck-pos-tip use-package counsel-projectile projectile yaml-mode which-key switch-window sunrise-x-tree sunrise-x-modeline sunrise-x-mirror sunrise-x-checkpoints solarized-theme smex project-local-variables paredit pandoc-mode ox-pandoc org-gcal org-download notify markdown-mode magit inf-ruby idris-mode idle-highlight-mode htmlize haskell-snippets graphviz-dot-mode git-gutter gist fsharp-mode find-file-in-project drag-stuff define-word cygwin-mount counsel caml calfw-gcal calfw auto-complete auctex ag)))
 '(switch-window-increase 6)
 '(switch-window-shortcut-style (quote qwerty)))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ediff-current-diff-A ((t :foreground "#E0E0E0" :background nil :inverse-video t)))
;;  '(ediff-current-diff-B ((t :foreground "#E0E0E0" :background nil :inverse-video t)))
;;  '(ediff-even-diff-A ((t :foreground "#E0E0E0" :background nil :inverse-video t)))
;;  '(ediff-even-diff-B ((t :foreground "#E0E0E0" :background nil :inverse-video t)))
;;  '(ediff-fine-diff-A ((t (:background nil :foreground "#EF9A9A" :weight bold))))
;;  '(ediff-fine-diff-B ((t (:background nil :foreground "#F4FF81" :weight bold))))
;;  '(ediff-odd-diff-A ((t :foreground "#E0E0E0" :background nil :inverse-video t)))
;;  '(ediff-odd-diff-B ((t :foreground "#E0E0E0" :background nil :inverse-video t)))
;;  '(org-todo ((t (:foreground "firebrick" :weight bold))))
;;  '(which-func ((t (:foreground "gold")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit nil :background "#fbf1d4" :family "Consolas"))))
 '(org-todo ((t (:foreground "firebrick" :weight bold))))
 '(which-func ((t (:foreground "gold")))))
