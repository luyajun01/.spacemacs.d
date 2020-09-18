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
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; (lsp :variables
     ;;      company-lsp-cache-candidates 'auto
     ;;      lsp-ui-flycheck t
     ;;      )
     (python :variables
             python-backend 'lsp
             python-lsp-server 'mspyls
             )
     (ivy :variables ivy-enable-advanced-buffer-information nil)
     ;better-defaults
     ;ranger
     ;; emoji
     ;(plantuml :variables plantuml-jar-path "~/.spacemacs.d/plantuml.jar")
     ;; lsp
     ;; dap
     colors
     ;; lpy
     ;; tabnine
     (ess :variables
          ess-enable-smart-equals t
          ;; ess-assign-key "M--"
          )
     imenu-list
     ;ipython-notebook
     ;prodigy
     ;; github
     search-engine
     ;graphviz
     ;(haskell :variables haskell-enable-hindent t
     ;         haskell-completion-backend 'intero)
     (syntax-checking :variables syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     (spell-checking :variables spell-checking-enable-by-default nil)
     ;; (vinegar :variables vinegar-reuse-dired-buffer t)
     (spacemacs-layouts :variables layouts-enable-autosave nil
                        layouts-autosave-delay 300)
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
                                                           ;; company-etags
                                                           ;; company-gtags
                                                           company-tabnine
                                                           company-files
                                                           company-capf
                                                           ;; company-tabnine
                                                           ;company-anaconda
                                                           company-keywords
                                                           ;company-yasnippet
                                                           company-dabbrev
                                                           )
                      auto-completion-idle-delay 0
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-help-tooltip t
                      auto-completion-use-company-box t
                      auto-completion-private-snippets-directory "~/dot-spacemacs/snippets/"
                      auto-completion-enable-snippets-in-popup t
                      ;; :disabled-for org markdown
                      )

     (osx :variables osx-dictionary-dictionary-choice "Simplified Chinese - English"
          osx-command-as 'super)
     ;restclient
     ;(gtags :disabled-for clojure emacs-lisp javascript latex python shell-scripts)
     ;; (shell :variables shell-default-shell 'ansi-term
     ;;        shell-default-term-shell "/bin/zsh")
     (shell :variables
            shell-default-term-shell "/bin/zsh" ;; find your zsh path using `$ whereis zsh`
            shell-default-height 30
            shell-default-position 'bottom)
     ;; docker
     latex
     ;deft
     markdown
     (org :variables org-want-todo-bindings t
          org-enable-hugo-support t)
     ;; gpu
     ;yaml
     ;react
     ;; (python :variables
             ;; python-test-runner '(nose pytest)
             ;; python-backend 'lsp
             ;python-lsp-server 'mspyls
             ;; python-lsp-git-root "~/Github/python-language-server")
     ;; (ruby :variables ruby-version-manager 'chruby)
     ;; ruby-on-rails
     ;lua
     ;html
     ;(javascript :variables javascript-backend 'lsp)
     ;(typescript :variables
     ;            typescript-fmt-on-save nil
     ;            typescript-fmt-tool 'typescript-formatter
     ;            typescript-backend 'lsp)
     emacs-lisp
     ;(clojure :variables clojure-enable-fancify-symbols t)
     ;racket
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-backend 'lsp-ccls
            c-c++-lsp-executable (file-truename "/usr/local/bin/ccls"))
     zilongshanren
     (chinese :variables chinese-default-input-method 'pinyin
              chinese-enable-youdao-dict t)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(;sicp
    major-mode-hydra
                                      diredfl
                                      solaire-mode
                                      doom-themes
                                      doom-modeline
                                      emmet-mode
                                      mixed-pitch
                                      ;iy-go-to-char
                                      ;ssh-agency
                                      ;format-all
                                      evil-textobj-syntax
                                      evil-lion
                                      evil-find-char-pinyin
                                      bash-completion
                                      pomodoro
                                      flymake-aspell
                                      ;; company-box
                                      keyfreq
                                      counsel-css
                                      ;; emojify
                                      ;fzf
                                     ivy-rich
                                     all-the-icons-dired
                                     ibuffer-vc
                                     company-prescient
                                     org-re-reveal
                                     ;; evil-escape
                                     evil-exchange
                                     ivy-xref
                                      ivy-posframe
                                      ;lsp-python-ms
                                      evil-org
                                      general
                                      keyfreq
                                      company-posframe
                                     company-quickhelp 
                                      window-numbering
                                      ace-jump-zap
                                      ;anaconda-mode
                                      evil-leader
                                      ;bm
                                      smex
                                      evil-nerd-commenter
                                      ;company-jedi
                                      ;company-anaconda
                                      tabbar
                                      toc-org
minions                                      
                                      ;sicp ssh-agency anki-editor
                                        powerline
                                        smartparens
                                      ;ranger
                                      ;; pophint
                                      all-the-icons
                                      ;xah-replace-pairs
                                      hydra
                                      ;spaceline-all-the-icons
                                        ;ace-window
                                      eval-in-repl
                                      flucui-themes
                                      ;; company-lsp
                                      ;jedi
                                      ivy-posframe
                                      flycheck-posframe
                                      company-posframe
                                      function-args
                                      elpy
                                      ;jupyter
                                      counsel-etags
                                      company-ctags
                                      mmm-mode
                                      lsp-mode
                                      lsp-python-ms
                                      lsp-ui
                                      slime
                                      helm-lsp
                                      lsp-treemacs
                                      ;; org-journal
                                      company-lsp
                                      ;; org-alert
                                        ;el2org
                                      cdlatex
                                      posframe ;emacs26.0以上使用
                                        ;flymd
                                        ;websocket
                                        ;simple-httpd
                                      ;; org-wiki
                                      ;plain-org-wiki
                                       company-tabnine
                                      ;; yasnippet-snippets
                                        ;rainbow-mode
                                        ;ivy-yasnippet
                                      cnfonts
                                      electric-operator
                                        ;leuven-theme
                                        ;helm-bm
                                      polymode
                                      poly-R
                                      poly-noweb
                                      poly-markdown
                                      poly-org
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages
   '(org-projectile org-brain magit-gh-pulls magit-gitflow  evil-mc realgud tern company-tern
                    evil-args evil-ediff evil-exchange evil-unimpaired
                    evil-indent-plus volatile-highlights
                    ;smartparens
                    spaceline holy-mode skewer-mode rainbow-delimiters
                    highlight-indentation vi-tilde-fringe eyebrowse ws-butler
                    ;org-bullets
                    smooth-scrolling org-repo-todo org-download org-timer
                    livid-mode git-gutter git-gutter-fringe
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
   dotspacemacs-enable-emacs-pdumper nil

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
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

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
                         monokai
                         ;; leuven
                         ;; solarized-light
                         ;underwater
;                         solarized-light
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
  )

(defun dotspacemacs/user-init ()
  ;; (setq-default configuration-layer-elpa-archives
  ;;               '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
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
;https://github.com/redguardtoo/emacs.d/blob/837c057ba17df5d157bdbd90e86103eaecc8eb9c/init.el
  ;;----------------------------------------------------------------------------
  ;; Which functionality to enable (use t or nil for true and false)
  ;;----------------------------------------------------------------------------
  (setq *is-a-mac* (eq system-type 'darwin))
  (setq *win64* (eq system-type 'windows-nt))
  (setq *cygwin* (eq system-type 'cygwin) )
  (setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
  (setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
  (setq *emacs24* (>= emacs-major-version 24))
  (setq *emacs25* (>= emacs-major-version 25))
  (setq *emacs26* (>= emacs-major-version 26))
  (setq *no-memory* (cond
                     (*is-a-mac*
                      (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                     (*linux* nil)
                     (t nil)))
  (defconst my-emacs-d (file-name-as-directory "~/.spacemacs.d/")
    "Directory of emacs.d")

  (defconst my-site-lisp-dir (concat my-emacs-d "site-lisp")
    "Directory of site-lisp")

  (defconst my-lisp-dir (concat my-emacs-d "lisp")
    "Directory of lisp")

  ;; *Message* buffer should be writable in 24.4+
  (defadvice switch-to-buffer (after switch-to-buffer-after-hack activate)
    (if (string= "*Messages*" (buffer-name))
        (read-only-mode -1)))
  
  (defun require-init (pkg &optional maybe-disabled)
    "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
    (when (or (not maybe-disabled) (not (boundp 'startup-now)))
      (load (file-truename (format "~/.spacemacs.d/lisp/%s" pkg)) t t)))

  (defun local-require (pkg)
    (unless (featurep pkg)
      (load (expand-file-name
             (cond
              ((eq pkg 'go-mode-load)
               (format "~/.spacemacs.d/site-lisp/go-mode/%s" pkg))
              (t
               (format "~/.spacemacs.d/site-lisp/%s/%s" pkg pkg))))
            t t)));; Load path
  ;; (add-to-list 'load-path (expand-file-name "lisp" "~/.emacs.d/"))

  ;; @see https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
  ;; Emacs 25 does gc too frequently
  ;; (setq garbage-collection-messages t) ; for debug
  (setq best-gc-cons-threshold (* 64 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)

  (defun my-vc-merge-p ()
    "Use Emacs for git merge only?"
    (boundp 'startup-now))
  (let* ((file-name-handler-alist nil))
  (require-init 'init-autoload)
  (require-init 'init-utils)
  ;; (require-init 'init-file-type)
  ;; (require-init 'init-elpa)
  (require-init 'init-spelling t)
  (require-init 'init-company t)
  (require-init 'init-company-new)
  (require-init 'init-linum-mode)
  ;; (require-init 'init-lsp)
  (require-init 'init-hippie-expand)
  (require-init 'init-lispyville)
  (require-init 'init-python t)
  (require-init 'init-targets)
 (require-init 'init-ibuffer t)
  (require-init 'init-keyfreq)
  (require-init 'init-theme)
  (require-init 'init-ivy)
  (require-init 'init-gtags t)
  (require-init 'init-git t)
  (require-init 'init-chinese t)
  (require-init 'init-markdown t)
  (require-init 'init-python)
  (require-init 'init-gtags t)
  (require-init 'init-ctags)
  (setq load-path (cdr load-path))
  (my-add-subdirs-to-load-path (file-name-as-directory my-site-lisp-dir))
  (require-init 'init-flymake t)
  (require-init 'init-evil)
  (require-init 'init-cc-mode t)
  (require-init 'init-clipboard)
  ;; (require-init 'init-dired t)
  (require-init 'init-dired-new)
  (require-init 'init-yasnippet)
  (require-init 'init-whichkey)
  (require-init 'init-const-new)
  (require-init 'init-windows-new)
  (require-init 'init-treemacs-new)
  (require-init 'init-hydra)
  (require-init 'init-hydra-new)
  (require-init 'init-ess)
  (require-init 'init-lispyville)
  (require-init 'init-essential)
  (require-init 'init-ui)
  ;; handy tools though not must have
  (require-init 'init-misc t)
  (require-init 'init-emacs-w3m t)
  (require-init 'init-shackle t)
  (require-init 'init-writting t)
  (require-init 'init-general)
  (require-init 'init-ediff)
  (require-init 'init-org t)
  (require-init 'init-workgroups2 t) ; use native API in lightweight mode
  (require-init 'init-term-mode t)
  ;; (require-init 'init-custom)
  )
  ;(add-to-list 'load-path "~/.spacemacs.d/extensions")
  ;(require 'ranger)
  (desktop-save-mode 1)
  ;; emacs 透明化 Transparency
  ;(spacemacs/enable-transparency)
  ;; 改变evil-insert-mode光标形状
  (setq-default evil-insert-state-cursor '("green" (bar . 2)))
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  ;;; lpy
  (add-to-list 'load-path "~/.spacemacs.d/private/lpy")
  (require 'lpy)
  ;;company-lsp
   ;; (with-eval-after-load 'lsp-mode
   ;;   (push '(company-lsp :with company-yasnippet) company-backends))

  ;; The package is "python" but the mode is "python-mode":
  (use-package python
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python" . python-mode))
  ;;plain-org-wiki
  ;; (add-to-list 'load-path "~/.spacemacs.d/private/plain-org-wiki")
  ;; (require 'plain-org-wiki)
  ;; (setq pow-directory "~/Documents/坚果云/我的坚果云/github/wiki/")

  ;;valign
  (add-to-list 'load-path "~/.spacemacs.d/private/valign")
  (require 'valign)
  (add-hook 'org-mode-hook 'valign-mode)

(use-package pyim
  :ensure nil
  :demand t
  :config
  ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))

  (setq default-input-method "pyim")

  ;; 我使用全拼
  (setq pyim-default-scheme 'pyim-shuangpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'popup)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)

  :bind
  (("C-S-P" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))
  
  ;;pdf-tools
;;; pdf-tools package and reinstall both as at the start.
  ;; (use-package pdf-tools
  ;;   :ensure t
  ;;   :config
  ;;   (custom-set-variables
  ;;    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  ;;   (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
  ;; (pdf-tools-install)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets";;personal snippets
        ))
(yas-global-mode 1)
                                        ;flycheck
  (global-flycheck-mode)
  ;;补充搜索括号特别有用
  (require 'evil-matchit)
  (global-evil-matchit-mode 1)
  ;; 文字自动转行
  (global-visual-line-mode 1)
  ;; 行号
  (global-linum-mode 1)
  ;;evil-leader-mode
  (window-numbering-mode 1)
  (require 'evil-surround)
  (global-evil-surround-mode)
  (evilnc-default-hotkeys)
  (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
;;color-rg
  (add-to-list 'load-path "~/.spacemacs.d/private/color-rg") ; add color-rg to your load-path
  (require 'color-rg)
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
  ;; ;; ;; dired-mode
  ;; ;; ;;返回上层目录，我绑定了快捷键i, 特别好按，非常流畅
  ;; ;; (add-hook 'dired-mode-hook
  ;; ;;           (lambda ()
  ;; ;;             (define-key dired-mode-map (kbd "i")
  ;; ;;               (lambda () (interactive) (find-alternate-file "..")))))

  ;;   ;;Chinese and English fonts alignment
  ;; (use-package cnfonts
  ;;   :config
  ;;   (cnfonts-enable)
  ;;   (setq cnfonts-use-face-font-rescale t)
  ;;   )
;;; R related modes
  (use-package polymode
    :mode
    (("\\.Rmd" . poly-markdown+r-mode))
    :init
    (autoload 'r-mode "ess-site.el" "Major mode for editing R source." t)
    :defer t
    )
                                        ;(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
                                        ;(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
                                        ;(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
                                        ;(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Snw$" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw$" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd$" .  poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.rapport$" . poly-rapport-mode))
  (add-to-list 'auto-mode-alist '("\\.Rhtml$" . poly-html+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rbrew$" . poly-brew+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rcpp$" . poly-r+c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppR$" . poly-c++r-mode))
  ;; (defun ess/init-polymode ()
  ;;   (use-package poly-R
  ;;     :defer t)
  ;;   (use-package poly-markdown
  ;;     :defer t)
  ;;   (use-package poly-markdown+r
  ;;     :defer t)
  ;;   ;;  ;   (use-package poly-noweb+r
  ;;   ;;  ; :defer t)
  ;;   )
  
  ;; 以下是zilongshanren配置
  ;; ;;解决org表格里面中英文对齐的问题
  ;; (when (configuration-layer/layer-usedp 'chinese)
  ;;   (when (and (spacemacs/system-is-mac) window-system)
  ;;     (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))
  ;; Setting Chinese Font
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; force horizontal split window
  (setq split-width-threshold 120)
  ;; (linum-relative-on)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  ;; temp fix for ivy-switch-buffer
  ;; (spacemacs/set-leader-keys "bb" 'helm-mini)
  (global-hungry-delete-mode t)
  (spacemacs|diminish helm-gtags-mode)
  (spacemacs|diminish ggtags-mode)
  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish spacemacs-whitespace-cleanup-mode)
  (spacemacs|diminish counsel-mode)

  (evilified-state-evilify-map special-mode-map :mode special-mode)

  (add-to-list 'auto-mode-alist
               '("Capstanfile\\'" . yaml-mode))

  ;; (defun js-indent-line ()
  ;;   "Indent the current line as JavaScript."
  ;;   (interactive)
  ;;   (let* ((parse-status
  ;;           (save-excursion (syntax-ppss (point-at-bol))))
  ;;          (offset (- (point) (save-excursion (back-to-indentation) (point)))))
  ;;     (if (nth 3 parse-status)
  ;;         'noindent
  ;;       (indent-line-to (js--proper-indentation parse-status))
  ;;       (when (> offset 0) (forward-char offset)))))

  (global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
  (defun un-indent-by-removing-4-spaces ()
    "remove 4 spaces from beginning of of line"
    (interactive)
    (save-excursion
      (save-match-data
        (beginning-of-line)
        ;; get rid of tabs at beginning of line
        (when (looking-at "^\\s-+")
          (untabify (match-beginning 0) (match-end 0)))
        (when (looking-at (concat "^" (make-string tab-width ?\ )))
          (replace-match "")))))

  (defun zilongshanren/toggle-major-mode ()
    (interactive)
    (if (eq major-mode 'fundamental-mode)
        (set-auto-mode)
      (fundamental-mode)))
  ;; (spacemacs/set-leader-keys "otm" 'zilongshanren/toggle-major-mode)
  (setq inhibit-compacting-font-caches t)
  (global-display-line-numbers-mode -1)

  (defun moon-override-yank-pop (&optional arg)
    "Delete the region before inserting poped string."
    (when (and evil-mode (eq 'visual evil-state))
      (kill-region (region-beginning) (region-end))))

  (advice-add 'counsel-yank-pop :before #'moon-override-yank-pop)
  (setq ivy-more-chars-alist '((counsel-ag . 2)
                               (counsel-grep .2)
                               (t . 3)))

  ;; boost find file and load saved persp layout  performance
  ;; which will break some function on windows platform
  ;; eg. known issues: magit related buffer color, reopen will fix it
  (defun counsel-locate-cmd-es (input)
    "Return a shell command based on INPUT."
    (counsel-require-program "es.exe")
    (encode-coding-string (format "es.exe -i -r -p %s"
                                  (counsel-unquote-regex-parens
                                   (ivy--regex input t)))
                          'gbk))
  ;; (add-hook 'text-mode-hook 'spacemacs/toggle-spelling-checking-on)
  ;; (add-hook 'org-mode-hook 'emojify-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)
;; (add-hook 'python-mode-hook 'anaconda-mode)
  ;; https://emacs-china.org/t/ox-hugo-auto-fill-mode-markdown/9547/4
  (defadvice org-hugo-paragraph (before org-hugo-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents)))

  ;; fix for the magit popup doesn't have a q keybindings
  (with-eval-after-load 'transient
    (transient-bind-q-to-quit))

  ;; fix for the lsp error
  (defvar spacemacs-jump-handlers-fundamental-mode nil)
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
