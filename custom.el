;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil t)
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(awesome-tray-mode t)
 '(command-log-mode-window-size 50)
 '(company-box-enable-icon t)
 '(company-box-icons-alist (quote company-box-icons-all-the-icons))
 '(company-box-show-single-candidate t)
 '(company-dabbrev-minimum-length 3)
 '(company-dabbrev-other-buffers nil)
 '(company-show-numbers t)
 '(company-statistics-auto-restore nil)
 '(compilation-message-face (quote default))
 '(counsel-search-engine (quote google))
 '(ctags-update-delay-seconds 1024)
 '(custom-safe-themes
   (quote
    ("5642b25b6df4d6b63787cbc3d3ef07ca4cb7b0a7a00740ce8e9867c00e57632f" "d548ac4bb4c8c0ba8f22476f5afcea11b7f1754065eefb118e1324f8a74883fb" default)))
 '(erc-nick "zilongshanren")
 '(erc-port 6666)
 '(evil-want-C-i-jump t)
 '(evil-want-Y-yank-to-eol nil)
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(expand-region-contract-fast-key "V")
 '(expand-region-exclude-text-mode-expansions (quote (html-mode nxml-mode web-mode)))
 '(expand-region-reset-fast-key "r")
 '(fci-rule-color "#3C3D37" t)
 '(git-gutter:handled-backends (quote (svn hg git)))
 '(global-command-log-mode nil)
 '(helm-buffer-max-length 56)
 '(helm-move-to-line-cycle-in-source t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(ivy-height 18)
 '(lsp-ui-flycheck-live-reporting nil)
 '(lua-documentation-url "http://www.lua.org/manual/5.3/manual.html")
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(only-global-abbrevs t)
 '(org-agenda-custom-commands nil)
 '(org-agenda-ndays 1)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-deadline-warning-days 14)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-log-into-drawer t)
 '(org-pomodoro-play-sounds nil)
 '(org-reverse-note-order t)
 '(package-selected-packages
   (quote
    (flucui-dark-theme ess-view-data ov org-pdftools org-noter org-edit-latex company-math math-symbol-lists ebib org-ref key-chord helm-bibtex bibtex-completion biblio parsebib biblio-core evil-collection annalist evil-easymotion isolate so-long evil-lispy color-moccur sdcv showtip bing-dict interleave rg pdf-view-restore csharp-mode swift-mode protobuf-mode org-roam-server org-roam emacsql-sqlite3 mocha lsp-sourcekit fish-mode counsel-osx-app vimrc-mode treemacs-persp sudo-edit smart-region shackle scala-mode rmsbolt quickrun powershell pomidor php-mode persp-mode-projectile-bridge persistent-scratch pager org-tree-slide org-rich-yank nov esxml modern-cpp-font-lock mermaid-mode magit-todos lsp-julia lsp-java howdoyou promise highlight-defined hide-mode-line helpful elisp-refs grip-mode goto-line-preview goto-last-point goto-char-preview gnu-elpa-keyring-update fancy-narrow esh-autosuggest easy-kill-extras easy-kill drag-stuff dired-rsync dired-quick-sort dired-git-info default-text-scale dashboard css-eldoc counsel-world-clock counsel-tramp comment-dwim-2 cask-mode calfw-org calfw-ical calfw bongo bmx-mode beginend avy-zap auto-package-update atomic-chrome all-the-icons-ivy-rich all-the-icons-ibuffer quelpa-use-package olivetti evil-snipe use-package-ensure-system-package system-packages ox-gfm pdf-tools highlight-indent-guides ob-ipython org-preview-html org-fancy-priorities ob-rust ob-mermaid ob-go lsp-pyright ivy-prescient ztree tldr memory-usage list-environment focus esup diffview daemons copyit frame-local minions all-the-icons-dired major-mode-hydra pretty-hydra mixed-pitch solaire-mode pyim-basedict xr flucui-light-theme lorem-ipsum flucui-themes company-lsp company-prescient prescient toc-org slime flymake-aspell pomodoro bash-completion ibuffer-vc org-re-reveal csv-mode amx lsp-ivy rime iy-go-to-char diredfl evil-find-char-pinyin evil-textobj-syntax company-childframe ivy-posframe flycheck-posframe doom-themes company-posframe format-all fzf ein skewer-mode lpy ess-smart-equals ess-R-data-view ess julia-mode xah-replace-pairs window-numbering tabbar posframe pophint yaxception poly-org poly-R poly-noweb poly-markdown polymode plain-org-wiki org-journal org-alert jupyter websocket zmq jedi auto-complete general function-args evil-leader eval-in-repl elpy electric-operator counsel-etags company-jedi jedi-core python-environment company-ctags company-box cdlatex bm ace-jump-zap cnfonts leetcode aio ycmd request-deferred treemacs-magit vterm attrap hybrid-mode lispyville anki-editor pyim pangu-spacing find-by-pinyin-dired chinese-wbim chinese-conv ace-pinyin pinyinlib python unicode-escape company-tabnine plantuml-mode dap-mode bui tree-mode lsp-ui lsp-treemacs lsp-python-ms helm-lsp cquery ccls forge closql emacsql-sqlite emacsql cpp-auto-include company-reftex parseedn lv parseclj a ox-hugo emojify emoji-cheat-sheet-plus company-emoji ob-typescript deadgrep org-cliplink devdocs ssh-agency org-projectile org-category-capture treemacs-projectile treemacs-evil treemacs pfuture transient osx-clipboard lsp-haskell lsp-mode flycheck-package package-lint evil-textobj-line blacken dante lcr company-ghc ghc helm-hoogle intero hlint-refactor hindent haskell-snippets flycheck-haskell company-ghci haskell-mode company-cabal cmm-mode imenu-list writeroom-mode visual-fill-column symbol-overlay treepy graphql sound-wav caps-lock doom-modeline eldoc-eval shrink-path ivy-rich prettier-js ivy-yasnippet gitignore-templates evil-goggles sesman dotenv-mode rjsx-mode magit-svn json-navigator hierarchy yasnippet-snippets spaceline-all-the-icons all-the-icons memoize pippel pipenv overseer org-mime nameless ivy-xref ivy-rtags importmagic epc concurrent google-c-style flycheck-rtags evil-cleverparens counsel-gtags counsel-css company-rtags rtags clojure-cheatsheet centered-cursor-mode font-lock+ ghub let-alist seq restclient-helm org-brain sayid evil-lion auctex-latexmk auctex password-generator realgud test-simple loc-changes load-relative company-lua blog-admin string-inflection opencl-mode cuda-mode symon rspec-mode fuzzy browse-at-remote winum helm-swoop unfill highlight-global marshal ht ob-restclient company-restclient know-your-http-well counsel-projectile lispy counsel swiper ivy-purpose hide-comnt helm-purpose window-purpose zoutline minitest glsl-mode pug-mode magithub editorconfig dockerfile-mode docker tablist docker-tramp helm-projectile xterm-color shell-pop eshell-z eshell-prompt-extras esh-help graphviz-dot-mode py-isort dumb-jump restclient racket-mode faceup projectile-rails ob-http helm-gtags feature-mode company-auctex rvm ruby-tools ruby-test-mode rubocop robe rbenv rake enh-ruby-mode chruby bundler inf-ruby yapfify sicp helm-mode-manager org origami tiny evil-unimpaired helm-pydoc unicode-whitespace github-search flycheck-clojure evil-escape mwim helm-github-stars fcitx solarized-theme tide typescript-mode spaceline powerline org-plus-contrib ivy-hydra helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-ag helm helm-core flyspell-correct-ivy color-identifiers-mode ag bracketed-paste paradox inflections cider names yaml-mode which-key wgrep uuidgen smex smeargle smartparens reveal-in-osx-finder restart-emacs ranger pytest py-yapf prodigy persp-mode pcre2el osx-trash org-pomodoro mmm-mode markdown-mode lua-mode live-py-mode link-hint launchctl js2-mode jade-mode info+ ibuffer-projectile projectile hy-mode htmlize hl-todo help-fns+ haml-mode gnuplot gitignore-mode github-clone popup git-gutter-fringe+ git-gutter+ flyspell-correct flycheck evil-visual-mark-mode evil-magit magit-popup git-commit with-editor evil-indent-plus iedit evil-ediff evil undo-tree diminish diff-hl ivy tern company column-enforce-mode cmake-mode clojure-snippets eval-sexp-fu pkg-info clojure-mode bind-map bind-key yasnippet auto-compile packed anaconda-mode pythonic ace-window ace-link avy quelpa package-build wrap-region visual-regexp-steroids visual-regexp peep-dired osx-dictionary nodejs-repl litable keyfreq gulpjs find-file-in-project etags-select ctags-update beacon 4clojure moe-theme edn paredit queue peg json-rpc dash-functional web-completion-data makey anzu highlight goto-chg flx gh logito pcache pos-tip guide-key request parent-mode simple-httpd json-snatcher json-reformat multiple-cursors moz ctable orglue epic alert log4e gntp spinner epl hydra async deferred f s chinese-word-at-point dash youdao-dictionary ws-butler web-mode web-beautify volatile-highlights vi-tilde-fringe use-package tagedit smooth-scrolling slim-mode scss-mode sass-mode rfringe reveal-in-finder rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pyenv-mode popwin pip-requirements persp-projectile pbcopy page-break-lines ox-reveal org-repo-todo org-present org-octopress org-mac-link org-download org-bullets open-junk-file neotree multi-term moz-controller move-text monokai-theme markdown-toc magit macrostep linum-relative leuven-theme less-css-mode json-mode js2-refactor js-doc indent-guide impatient-mode ido-vertical-mode hungry-delete hl-anything highlight-parentheses highlight-numbers highlight-indentation guide-key-tip google-translate golden-ratio github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh-md ggtags geiser fringe-helper flycheck-ycmd flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-tutor evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-args evil-anzu engine-mode emmet-mode elisp-slime-nav elfeed discover-my-major deft dash-at-point cython-mode company-ycmd company-web company-tern company-statistics company-quickhelp company-c-headers company-anaconda command-log-mode coffee-mode cmake-font-lock clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu chinese-fonts-setup buffer-move auto-yasnippet auto-highlight-symbol auto-dictionary align-cljlet aggressive-indent adaptive-wrap ace-jump-mode ac-ispell 2048-game)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((org-confirm-babel-evaluate)
     (git-commit-major-mode . git-commit-elisp-text-mode)
     (typescript-backend . tide)
     (javascript-backend . tern)
     (eval progn
           (pp-buffer)
           (indent-buffer))
     (typescript-backend . lsp)
     (javascript-backend . lsp)
     (eval setenv "PYTHONPATH" "/Users/guanghui/cocos2d-x/tools/cocos2d-console/plugins:/Users/guanghui/cocos2d-x/tools/cocos2d-console/bin"))))
 '(sp-show-pair-from-inside t)
 '(spaceline-all-the-icons-icon-set-eyebrowse-slot (quote solid))
 '(spaceline-all-the-icons-icon-set-git-ahead (quote commit))
 '(spaceline-all-the-icons-icon-set-modified (quote toggle))
 '(spaceline-all-the-icons-separator-type (quote arrow))
 '(spaceline-all-the-icons-slim-render t)
 '(spaceline-all-the-icons-window-number-always-visible t)
 '(tags-revert-without-query t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(web-mode-markup-indent-offset 2)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
 '(ycmd-extra-conf-handler (quote load))
 '(ycmd-extra-conf-whitelist (quote ("~/cocos2d-x/*"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "nil" :slant normal :weight normal :height 180 :width normal))))
 '(awesome-tray-module-battery-face ((t (:foreground "#00ced1")))))
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
