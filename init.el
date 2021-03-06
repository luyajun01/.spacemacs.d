;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation nil
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation nil
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/dot-spacemacs")
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; (lsp :variables
     ;;      company-lsp-cache-candidates 'auto
     ;;      lsp-ui-flycheck t
     ;;      )
command-log
    (python :variables
             python-backend 'anaconda
             ;; python-lsp-server 'pyright
             )
     (ivy :variables ivy-enable-advanced-buffer-information nil)
     ;better-defaults
     ;ranger
     ;; emoji
     ;(plantuml :variables plantuml-jar-path "~/.spacemacs.d/plantuml.jar")
     ;; lsp
     ;; dap
     ;; colors
;; (python :variables
;;        python-backend 'lsp
;;         python-formatter 'yapf
;;         python-format-on-save t
;;         )
     lpy
    ;; tabnine
     (ess :variables
          ess-enable-smart-equals t
          ;; ess-assign-key "M--"
          )
     imenu-list
;; pdf-tools
;; search-engine
;; (elfeed :variables
;;         elfeed-feeds '(("http://nullprogram.com/feed/" blog emacs)
;;                        "http://www.50ply.com/atom.xml"  ; no autotagging
;;                        ("http://nedroid.com/feed/" webcomic)))
     ;ipython-notebook
     ;prodigy
     ;; spacemacs-language
     ;; search-engine
     (syntax-checking :variables syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     (spell-checking :variables spell-checking-enable-by-default nil)
     ;; (vinegar :variables vinegar-reuse-dired-buffer t)
     ;; (spacemacs-layouts :variables layouts-enable-autosave nil
     ;;                    layouts-autosave-delay 300)
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)
     ;(ibuffer :variables ibuffer-group-buffers-by 'projects)
     ;; (auto-completion :variables auto-completion-enable-sort-by-usage t
     ;;                  auto-completion-enable-snippets-in-popup t
     ;;                  auto-completion-tab-key-behavior 'cycle
     ;;                  :disabled-for org markdown)
     (auto-completion :variables
                      spacemacs-default-company-backends '(
                                                           (company-anaconda company-capf)
                                                           (company-dabbrev-code company-gtags company-etags company-keywords)
                                                           company-files company-dabbrev
                                                           ;; company-etags
                                                           ;; company-gtags
                                                           ;; company-tabnine
                                                           ;; company-files
                                                           ;; company-capf
                                                           ;; company-tabnine
                                                           ;; company-anaconda
                                                           ;; company-keywords
                                                           ;; company-yasnippet
                                                           ;; company-dabbrev
                                                           )
                      auto-completion-idle-delay 0
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.3
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-help-tooltip t
                      auto-completion-use-company-box t
                      auto-completion-private-snippets-directory "~/dot-spacemacs/snippets/"
                      auto-completion-enable-snippets-in-popup t
                      ;; :disabled-for org markdown
                      )

     ;; (osx :variables osx-dictionary-dictionary-choice "Simplified Chinese - English"
     ;;      osx-command-as 'super)
     ;restclient
     ;(gtags :disabled-for clojure emacs-lisp javascript latex python shell-scripts)
     ;; (shell :variables shell-default-shell 'ansi-term
     ;;        shell-default-term-shell "/bin/zsh")
     (shell :variables
            shell-default-term-shell "/bin/zsh" ;; find your zsh path using `$ whereis zsh`
            shell-default-height 30
            shell-default-position 'bottom)
     ;; docker
     (latex :variables
            latex-build-command "XeLaTeX"
            latex-enable-folding t
            latex-enable-magic t
            )
     ;deft
     ;; markdown
     ;; (org :variables org-want-todo-bindings t
     ;;      org-enable-hugo-support t)
     ;; gpu
     ;yaml
     ;react
     ;; (python :variables
     ;;          python-test-runner '(nose pytest)
     ;;          python-backend 'lsp
     ;;          python-lsp-server 'pyls
     ;;          python-lsp-git-root "~/Github/python-language-server")
     ;; (ruby :variables ruby-version-manager 'chruby)
     ;; ruby-on-rails
     ;lua
     ;html
     ;(javascript :variables javascript-backend 'lsp)
     (bibtex :variables
             bibtex-completion-pdf-field "file"
             ;;如果你想用 ivy 进行文献的插入管理，请使用 org-ref-default-bibliography
             org-ref-default-bibliography 'org-ref-ivy-cite
             bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
             bibtex-completion-library-path "~/Dropbox/bibliography"
             org-ref-notes-directory "~/Dropbox/bibliography"
             org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
             bibtex-completion-notes "~/Dropbox/bibliography/notes.org"
             org-ref-pdf-directory "~/Dropbox/bibliography/notes.org"
             org-ref-default-bibliography "~/Dropbox/bibliography/references.bib"
             )
     ;; (typescript :variables
     ;;             typescript-fmt-on-save nil
     ;;             typescript-fmt-tool 'typescript-formatter
     ;;             typescript-backend 'lsp)
     ;; emacs-lisp
     ;(clojure :variables clojure-enable-fancify-symbols t)
     ;racket
      ;; (c-c++ :variables
      ;;        c-c++-default-mode-for-headers 'c++-mode
      ;;        c-c++-backend 'lsp-ccls
      ;;        c-c++-lsp-executable (file-truename "/usr/local/bin/ccls"))
     ;; zilongshanren
     ;; (chinese :variables chinese-default-input-method 'pinyin
     ;;          chinese-enable-youdao-dict t)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages ;; A list of packages that cannot be updated.
   '(                               ;sicp
     pyim
     ;; xterm-color
     ;; sunburn-theme
     ;; grip-mode
     dash-at-point
     ;; flycheck-popup-tip
     pretty-symbols
     ;; dracula-theme
     ;; anaconda-mode
     company-anaconda
     ;; live-py-mode
;; markdown-toc
     ;; ov
     ;; rime
     ace-pinyin
     ;; pinyinlib
     ;; fcitx
     ;; sr-speedbar
     ;; ess-view-data
     ;; go-translate
     ;; treemacs
     ;; ov
     ;; ebib
     org-ref
     ;; org-pdftools
     ;; org-brain
     auctex
     company-math
     cdlatex
     org-edit-latex
                                        ; so-long
     ;; bing-dict
     ;; color-moccur
     evil-lispy
     ;; sdcv
     ;; google-translate
     ;; quelpa
     ;; interleave
     lispy
     ;; quelpa-use-package
     rg
     ;; json-mode
     ;; skewer-mode
     ;; csharp-mode
     ;; pdf-view-restore
     ;; haml-mode
     ;; prettier-js
     ;; evil-snipe
     ;; mocha
     ;; forge
     ;; js2-mode
     ;; scss-mode
     ;; fish-mode
     ;; protobuf-mode
     ;; swift-mode
     ;; lsp-sourcekit
     ;; browse-at-remote
     ;; org-roam
     ;; org-roam-server
     ;; counsel-osx-app
     ;; dired-quick-sort
     rainbow-delimiters
     ;; volatile-highlights
     all-the-icons-ibuffer
     major-mode-hydra
                                        ; ivy-prescient
     evil-escape
     ;; gnu-elpa-keyring-update
     ;; auto-package-update
     ;; hide-mode-line
     ;; default-text-scale
     ;; goto-line-preview
     ;; avy-zap
     ;; dired-git-info
     ;; dired-rsync
     ;; symbol-overlay
     ;; highlight-indent-guides
     ;; diff-hl
     ;; volatile-highlights
     ;; ibuffer-projectile
     ;; easy-kill-extras
     ;; shackle
     ;; ob-go
     ;; ob-rust
     ob-ipython
     ;; ob-mermaid
     ;; beginend
     ;; comment-dwim-2
     ;; drag-stuff
     multiple-cursors
     ;; smart-region
     ;; mwim
     ;; pager
     ;; goto-last-point
     ;; goto-char-preview
     ;; sudo-edit
     ;; fancy-narrow
     ;; goto-line-preview
     ;; amx
     ;; ivy-yasnippet
     ;; counsel-world-clock
     ;; counsel-tramp
     ;; all-the-icons-ivy-rich
     yasnippet-snippets
     ;; calfw
     ;; calfw-org
     ;; calfw-ical
     ;; dashboard
     company-box
     ;; diredfl
     ;; solaire-mode
     ;; doom-themes
     ;; doom-modeline
     ;; emmet-mode
     ;; mixed-pitch
                                        ;iy-go-to-char
                                        ;ssh-agency
                                        ;format-all
     ;; evil-textobj-syntax
     ;; evil-lion
     ;; evil-find-char-pinyin
     ;; bash-completion
     ;; pomodoro
     flymake-aspell
     keyfreq
     ;; counsel-css
     ;; emojify
                                        ;fzf
     ;; ivy-rich
     ;; all-the-icons-dired
     ;; ibuffer-vc
     ;; company-prescient
     ;; org-re-reveal
     ;; evil-escape
     ;; evil-exchange
     ;; ivy-xref
     ;; ivy-posframe
                                        ;lsp-python-ms
     evil-org
     general
     ;; keyfreq
     company-posframe
     ;; company-quickhelp
     ;; window-numbering
     ;; ace-jump-zap
                                        ;anaconda-mode
     evil-leader
                                        ;bm
     ;; smex
     evil-nerd-commenter
                                        ;company-jedi
                                        ;company-anaconda
     ;; tabbar
     toc-org
     ;; minions
                                        ;sicp ssh-agency anki-editor
     ;; powerline
     smartparens
                                        ;ranger
     ;; pophint
     all-the-icons
                                        ;xah-replace-pairs
     hydra
                                        ;spaceline-all-the-icons
                                        ;ace-window
     eval-in-repl
     ;; flucui-themes
     ;; company-lsp
                                        ;jedi
     ;; ivy-posframe
     flycheck-posframe
     ;; company-posframe
     ;; function-args
     elpy
     ;; jupyter
     ;; counsel-etags
     ;; company-ctags
     ;; mmm-mode
     ;; lsp-mode
     ;; lsp-python-ms
     ;; lsp-ui
     ;; slime
     ;; helm-lsp
     ;; lsp-treemacs
     ;; org-journal
     ;; company-lsp
     ;; elfeed
     ;; olivetti
     ;; bongo
     ;; ztree
     ;; diffview
     ;; memory-usage
     ;; howdoyou
     ;; esup
     ;; focus
     ;; tldr
     ;; ccls
     ;; lsp-julia
     ;; lsp-java
     ;; quickrun
     ;; cask-mode
     ;; csv-mode
     ;; lua-mode
     ;; mermaid-mode
     ;; rmsbolt
     ;; scala-mode
     ;; bmx-mode
     ;; helpful
     ;; overseer
     ;; modern-cpp-font-lock
     ;; daemons
     ;; editorconfig
     ;; list-environment
     ;; css-eldoc
     ;; coffee-mode
     ;; web-mode
     ;; php-mode
     ;; restclient
     ;; plantuml-mode
     ;; org-rich-yank
     ;; org-fancy-priorities
     ;; esh-autosuggest
     ;; treemacs-projectile
     ;; treemacs-magit
     ;; treemacs-persp
     ;; grip-mode
     ;; nov
     ;; lsp-ivy
     ;; lsp-pyright
     ;; dap-mode
     ;; powershell
     ;; vimrc-mode
     ;; evil-exchange
     ;; org-alert
                                        ;el2org
     ;; cdlatex
     posframe                           ;emacs26.0以上使用
                                        ;flymd
                                        ;websocket
                                        ;simple-httpd
                                        ;plain-org-wiki
     ;; magit-todos
     ;; copyit
     ;; atomic-chrome
     ;; pomidor
     ;; persistent-scratch
     ;; org-tree-slide
     org-preview-html
     company-tabnine
     ;; yasnippet-snippets
                                        ;rainbow-mode
                                        ;ivy-yasnippet
     cnfonts
     electric-operator
     ;; persp-mode-projectile-bridge
     ;; highlight-defined
                                        ;leuven-theme
                                        ;helm-bm
     poly-org
     polymode
     poly-R
     git-gutter
     ;; poly-noweb
     ;; poly-markdown
     )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages
   '(org-projectile org-brain magit-gh-pulls magit-gitflow  evil-mc realgud tern company-tern
                    evil-args evil-ediff evil-exchange evil-unimpaired
                    evil-indent-plus
                    ;volatile-highlights
                    ;smartparens
                    spaceline holy-mode
                    ;skewer-mode
                    ;rainbow-delimiters
                    highlight-indentation vi-tilde-fringe eyebrowse ws-butler
                    org-bullets
                    smooth-scrolling org-repo-todo org-download org-timer
                    livid-mode git-gutter-fringe
                    ;; evil-escape
                    ;leuven-theme gh-md evil-lisp-state spray lorem-ipsum symon
                    ac-ispell ace-jump-mode auto-complete auto-dictionary
                    clang-format define-word google-translate disaster epic
                    fancy-battery org-present orgit orglue spacemacs-theme
                    helm-flyspell flyspell-correct-helm clean-aindent-mode
                    helm-c-yasnippet ace-jump-helm-line helm-make magithub
                    helm-themes helm-swoop helm-spacemacs-help smeargle
                    ido-vertical-mode flx-ido company-quickhelp
                                        ;ivy-rich
                    helm-purpose
                    )
   dotspacemacs-install-packages 'used-only
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper t

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "~/.spacemacs.d/emacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 300

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         ;; flucui-dark
;; tsdh-light
                         ;; monokai
                         leuven
                         ;; solarized-light
                         ;underwater
                         ;; solarized-light
                                        ;solarized-dark
                         )
   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 18
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  (defun org/pre-dump ()
    (spacemacs/dump-modes '(
                           python-mode org-mode emacs-lisp-mode c++-mode web-mode magit-status-mode
                            )))
  )

(defun dotspacemacs/user-init ()
  ;; (setq-default configuration-layer-elpa-archives
  ;;              '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
  ;;                 ("org-cn"   . "http://elpa.emacs-china.org/org/")
  ;;                 ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))
  (setq configuration-layer--elpa-archives
        '(("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
          ("org-cn"   . "http://mirrors.cloud.tencent.com/elpa/org/")
          ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")))

  (setq term-char-mode-point-at-process-mark nil)

  ;; https://github.com/syl20bnr/spacemacs/issues/2705
  ;; (setq tramp-mode nil)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; ss proxy. But it will cause anacond-mode failed.
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (setq evil-shift-round nil)
  (setq byte-compile-warnings '(not obsolete))
  (setq warning-minimum-level :error)

  ;; https://github.com/syl20bnr/spacemacs/issues/8901
  (setq-default quelpa-build-tar-executable "/usr/local/bin/gtar")
  ;; hack for remove purpose mode
  ;; (setq purpose-mode nil)
  )

(defun dotspacemacs/user-config ()
   (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  ;; (require 'smex)                       ; Not needed if you use package.el
  ;; (smex-initialize)
  ;; (global-set-key (kbd "M-x") 'smex)
  ;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;;https://emacs-china.org/t/lazy-load-el-emacs/9208

  (let (;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
        (gc-cons-threshold most-positive-fixnum)
        ;; 清空避免加载远程文件的时候分析文件。
        (file-name-handler-alist nil))
    ;;lazy-load
    (add-to-list 'load-path (expand-file-name "~/.spacemacs.d/private/lazy-load"))
    (require 'lazy-load)

    (cnfonts-enable)
    (elpy-enable)

    (when (version< emacs-version "25.1")
      (error "This requires Emacs 25.1 and above!"))

    ;; Speed up startup
    (defvar centaur-gc-cons-threshold (if (display-graphic-p) 16000000 1600000)
      "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

    (defvar centaur-gc-cons-upper-limit (if (display-graphic-p) 400000000 100000000)
      "The temporary value for `gc-cons-threshold' to defer it.")

    (defvar centaur-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
      "Run garbarge collection when idle 10s.")

    (defvar default-file-name-handler-alist file-name-handler-alist)

    (setq file-name-handler-alist nil)
    (setq gc-cons-threshold centaur-gc-cons-upper-limit
          gc-cons-percentage 0.5)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Restore defalut values after startup."
                (setq file-name-handler-alist default-file-name-handler-alist)
                (setq gc-cons-threshold centaur-gc-cons-threshold
                      gc-cons-percentage 0.1)

                ;; GC automatically while unfocusing the frame
                ;; `focus-out-hook' is obsolete since 27.1
                (if (boundp 'after-focus-change-function)
                    (add-function :after after-focus-change-function
                                  (lambda ()
                                    (unless (frame-focus-state)
                                      (garbage-collect))))
                  (add-hook 'focus-out-hook 'garbage-collect))

                ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
                ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
                (defun my-minibuffer-setup-hook ()
                  (setq gc-cons-threshold centaur-gc-cons-upper-limit))

                (defun my-minibuffer-exit-hook ()
                  (setq gc-cons-threshold centaur-gc-cons-threshold))

                (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
                (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

    ;; Load path
    ;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
    (defun update-load-path (&rest _)
      "Update `load-path'."
      (dolist (dir '("site-lisp" "lisp"))
        (push (expand-file-name dir "~/.spacemacs.d/") load-path)))

    (defun add-subdirs-to-load-path (&rest _)
      "Add subdirectories to `load-path'."
      (let ((default-directory (expand-file-name "site-lisp" "~/.spacemacs.d/")))
        (normal-top-level-add-subdirs-to-load-path)))

    (advice-add #'package-initialize :after #'update-load-path)
    (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

    (update-load-path)
    ;; Packages
    ;; Without this comment Emacs25 adds (package-initialize) here
    (require 'init-python)
    (require 'init-private)
    ;; (require 'init-scimax-ob)
    ;; (require 'init-scimax-org-latex)
    ;; (require 'init-rime)
    ;; (require 'init-c)
    ;; (require 'init-lsp)
    (require 'init-utils)
    (require 'init-git)
    (require 'init-org)
    (require 'init-evil)
    ;; (require 'init-custom)
    ;; (require 'init-ui)
    ;; (require 'init-google-translate)
    ;; (require 'init-doi)
    (require 'init-one-key)
    ;; (require 'init-shell)
    (require 'init-key)
    ;; (require 'init-symbol-overlay)
    ;; (require 'init-mode)
    ;; (require 'init-pdftools)
    (require 'init-thing-edit)
    (require 'init-company)
    ;; (require 'init-package)
    ;; (require 'init-speedbar)
    ;; Preferences
    ;; (require 'init-basic)
    (require 'init-hydra)
    ;; (require 'init-line-number)
    ;; (require 'init-edit)
    (require 'init-ivy)
    (require 'init-yasnippet)
    ;; (require 'init-scimax-org-babel)
    (require 'init-dired)
    (require 'init-highlight)
    ;; (require 'init-ibuffer)
    (require 'init-kill-ring)
    ;; (require 'init-window)
    ;; (require 'init-treemacs)
    (require 'init-ess)
    ;; (require 'init-smex)
    ;; (require 'init-markdown)
    ;; Programming
    (require 'init-flycheck)
    ;; (require 'init-projectile)
    ;; (require 'init-company-tabnine)
    ;; (require 'init-elisp)
    ;; 改变evil-insert-mode光标形状
    (setq-default evil-insert-state-cursor '("green" (bar . 2)))
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    ;; ;;; lpy
    ;; (add-to-list 'load-path "~/.spacemacs.d/private/lpy")
    ;; (require 'lpy)
    ;;company-lsp
    ;; (with-eval-after-load 'lsp-mode
    ;;   (push '(company-lsp :with company-yasnippet) company-backends))

    ;; The package is "python" but the mode is "python-mode":
    ;;plain-org-wiki
    ;; (add-to-list 'load-path "~/.spacemacs.d/private/plain-org-wiki")
    ;; (require 'plain-org-wiki)
    ;; (setq pow-directory "~/Documents/坚果云/我的坚果云/github/wiki/")

    ;;valign
    ;; (add-to-list 'load-path "~/.spacemacs.d/private/valign")
    ;; (require 'valign)
    ;; (add-hook 'org-mode-hook 'valign-mode)
    ;; (use-package pyim
    ;;   :ensure nil
    ;;   :demand t
    ;;   :config
    ;;   ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
    ;;   (use-package pyim-basedict
    ;;     :ensure nil
    ;;     :config (pyim-basedict-enable))

    ;;   (setq default-input-method "pyim")

    ;;   ;; 我使用全拼
    ;;   (setq pyim-default-scheme 'pyim-shuangpin)

    ;;   ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
    ;;   ;; 我自己使用的中英文动态切换规则是：
    ;;   ;; 1. 光标只有在注释里面时，才可以输入中文。
    ;;   ;; 2. 光标前是汉字字符时，才能输入中文。
    ;;   ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
    ;;   (setq-default pyim-english-input-switch-functions
    ;;                 '(pyim-probe-dynamic-english
    ;;                   pyim-probe-isearch-mode
    ;;                   pyim-probe-program-mode
    ;;                   pyim-probe-org-structure-template))

    ;;   (setq-default pyim-punctuation-half-width-functions
    ;;                 '(pyim-probe-punctuation-line-beginning
    ;;                   pyim-probe-punctuation-after-punctuation))

    ;;   ;; 开启拼音搜索功能
    ;;   (pyim-isearch-mode 1)

    ;;   ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
    ;;   ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
    ;;   ;; 手动安装 posframe 包。
    ;;   (setq pyim-page-tooltip 'popup)

    ;;   ;; 选词框显示5个候选词
    ;;   (setq pyim-page-length 5)

    ;;   :bind
    ;;   (("C-S-P" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
    ;;    ("C-;" . pyim-delete-word-from-personal-buffer)))
    ;; load theme
    ;;pdf-tools
;;; pdf-tools package and reinstall both as at the start.
    ;; (use-package pdf-tools
    ;;   :ensure t
    ;;   :config
    ;;   (custom-set-variables
    ;;    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
    ;;   (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
    ;; (pdf-tools-install)
    (global-flycheck-mode)
    ;;补充搜索括号特别有用
    (require 'evil-matchit)
    (global-evil-matchit-mode 1)
    ;; 文字自动转行
    (global-visual-line-mode 1)
    ;; 行号
    (global-linum-mode 1)
    ;;evil-leader-mode
    ;; (window-numbering-mode 1)
    (require 'evil-surround)
    (global-evil-surround-mode)
    (evilnc-default-hotkeys)
    (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    ;; ;;color-rg
    ;;   (add-to-list 'load-path "~/.spacemacs.d/private/color-rg") ; add color-rg to your load-path
    ;;   (require 'color-rg)
    ;; ;;   ;; ivy
    ;; ;; ;; (use-package ivy
    ;; ;; ;;   :ensure t
    ;; ;; ;;   :defer t
    ;; ;; ;;   :diminish ivy-mode
    ;; ;; ;;   :config
    ;; ;; ;;   (progn
    ;; ;; ;;     (ivy-mode)
    ;; ;; ;;     (with-eval-after-load 'recentf
    ;; ;; ;;       (setq ivy-use-virtual-buffers t))
    ;; ;; ;;     (setq ivy-height 12
    ;; ;; ;;           ivy-do-completion-in-region nil
    ;; ;; ;;           ivy-wrap t
    ;; ;; ;;           ivy-extra-directories nil
    ;; ;; ;;           ivy-fixed-height-minibuffer t
    ;; ;; ;;           ;; Don't use ^ as initial input
    ;; ;; ;;           ivy-initial-inputs-alist nil
    ;; ;; ;;           ;; highlight til EOL
    ;; ;; ;;           ivy-format-function #'ivy-format-function-line
    ;; ;; ;;           ;; disable magic slash on non-match
    ;; ;; ;;           ;; ~ to /home/user
    ;; ;; ;;           ;; ivy-magic-tilde nil
    ;; ;; ;;           ivy-magic-slash-non-match-action nil)
    ;; ;; ;;     ;; (setq ivy-re-builders-alist
    ;; ;; ;;     ;;       '((t . ivy--regex-fuzzy)))
    ;; ;; ;;     ;; (setq confirm-nonexistent-file-or-buffer t)
    ;; ;; ;;     (setq ivy-re-builders-alist
    ;; ;; ;;           '((t   . ivy--regex-ignore-order)))
    ;; ;; ;;     (evil-make-overriding-map ivy-occur-mode-map 'normal)
    ;; ;; ;;     ))

    )
  )
(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  )

(provide 'init)
