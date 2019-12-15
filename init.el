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
     (ivy :variables ivy-enable-advanced-buffer-information nil)
     better-defaults
     ranger
     emoji
     (plantuml :variables plantuml-jar-path "~/.spacemacs.d/plantuml.jar")
     ;; lsp
     ;; dap
     colors
     lpy
     tabnine
     (ess :variables
          ess-enable-smart-equals t
          ess-assign-key "M--"
          )
     imenu-list
     ipython-notebook
     prodigy
     ;; github
     search-engine
     graphviz
     (haskell :variables haskell-enable-hindent t
              haskell-completion-backend 'intero)
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
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     ;; (auto-completion :variables auto-completion-enable-sort-by-usage t
     ;;                  auto-completion-enable-snippets-in-popup t
     ;;                  auto-completion-tab-key-behavior 'cycle
     ;;                  :disabled-for org markdown)
     (auto-completion :variables
                      spacemacs-default-company-backends '(company-etags
                                                           company-gtags
                                                           company-files
                                                           company-capf
                                                           ;; company-anaconda
                                                           company-keywords
                                                           ;; company-yasnippet
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
     restclient
     (gtags :disabled-for clojure emacs-lisp javascript latex python shell-scripts)
     (shell :variables shell-default-shell 'ansi-term
            shell-default-term-shell "/bin/zsh")
     ;; docker
     latex
     deft
     markdown
     (org :variables org-want-todo-bindings t
          org-enable-hugo-support t)
     gpu
     yaml
     react
     (python :variables
             python-test-runner '(nose pytest)
             python-backend 'lsp
             python-lsp-server 'mspyls
             python-lsp-git-root "~/Github/python-language-server")
     ;; (ruby :variables ruby-version-manager 'chruby)
     ;; ruby-on-rails
     lua
     html
     (javascript :variables javascript-backend 'lsp)
     (typescript :variables
                 typescript-fmt-on-save nil
                 typescript-fmt-tool 'typescript-formatter
                 typescript-backend 'lsp)
     emacs-lisp
     (clojure :variables clojure-enable-fancify-symbols t)
     racket
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
   dotspacemacs-additional-packages '(sicp
                                      ssh-agency
                                      anki-editor
                                      company-box
                                      keyfreq
                                      emojify
                                      fzf
                                      lsp-python-ms
                                      ;; evil-org
                                      general
                                      window-numbering
                                      ace-jump-zap
                                      ;; evil-leader
                                      bm
                                      company-jedi
                                      tabbar
                                      sicp ssh-agency anki-editor
                                        ;powerline
                                        ;smartparens
                                      ranger
                                      ;; pophint
                                      ;; all-the-icons
                                      xah-replace-pairs
                                      hydra
                                      ;; spaceline-all-the-icons
                                        ;ace-window
                                      eval-in-repl
                                      lispy
                                      jedi
                                      function-args
                                      elpy
                                      jupyter
                                      ;; counsel-etags
                                      ;; company-ctags
                                      mmm-mode
                                        ;lsp-mode
                                        ;lsp-ui
                                      ;; org-journal
                                      ;; company-lsp
                                      org-alert
                                        ;el2org
                                      cdlatex
                                      posframe ;emacs26.0以上使用
                                        ;flymd
                                        ;websocket
                                        ;simple-httpd
                                      ;; org-wiki
                                      plain-org-wiki
                                        ;real-auto-save
                                      ;; company-tabnine
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
                    evil-indent-plus volatile-highlights smartparens
                    spaceline holy-mode skewer-mode rainbow-delimiters
                    highlight-indentation vi-tilde-fringe eyebrowse ws-butler
                    org-bullets smooth-scrolling org-repo-todo org-download org-timer
                    livid-mode git-gutter git-gutter-fringe  evil-escape
                    leuven-theme gh-md evil-lisp-state spray lorem-ipsum symon
                    ac-ispell ace-jump-mode auto-complete auto-dictionary
                    clang-format define-word google-translate disaster epic
                    fancy-battery org-present orgit orglue spacemacs-theme
                    helm-flyspell flyspell-correct-helm clean-aindent-mode
                    helm-c-yasnippet ace-jump-helm-line helm-make magithub
                    helm-themes helm-swoop helm-spacemacs-help smeargle
                    ido-vertical-mode flx-ido company-quickhelp ivy-rich helm-purpose
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
   dotspacemacs-themes '(;; leuven
                         underwater
                         solarized-light
                         solarized-dark)
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
                               :size 14
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
  (setq-default configuration-layer-elpa-archives
                '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
                  ("org-cn"   . "http://elpa.emacs-china.org/org/")
                  ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

  
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
  ;; Transparency
  (spacemacs/enable-transparency)
  ;;company
  ;; company-english
  ;;  helm-company choose from company completions with C-:
  (with-eval-after-load 'company
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company)
    (setq ess-use-company t)
    ;; (require 'company-tabnine);以后要用的时候，再打开
    ;; (add-to-list 'company-backends #'company-tabnine)
                                        ;以后要用的时候打开
    ;; (setq company-idle-delay 0)
    (setq company-show-numbers t)
    (setq company-frontends
          '(company-tng-frontend
            company-pseudo-tooltip-frontend
            company-echo-metadata-frontend))
    ;; Add yasnippet support for all company backends
    ;; https://github.com/syl20bnr/spacemacs/pull/179
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")
    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)
          )
    ;;company-english-
    ;; (add-to-list 'load-path (expand-file-name "~/dot-spacemacs/private/company-english-helper"))
    ;; (require 'company-english-helper)
    ;; (setq company-english-helper-fuzz-search-p t);模糊匹配
    ;; (add-hook 'after-init-hook 'company-statistics-mode)
    ;;        ;; workaround for company-transformers
    ;; (setq company-tabnine--disable-next-transform nil)
    ;; (defun my-company--transform-candidates (func &rest args)
    ;;   (if (not company-tabnine--disable-next-transform)
    ;;       (apply func args)
    ;;     (setq company-tabnine--disable-next-transform nil)
    ;;     (car args)))
    ;; (defun my-company-tabnine (func &rest args)
    ;;   (when (eq (car args) 'candidates)
    ;;     (setq company-tabnine--disable-next-transform t))
    ;;   (apply func args))

    ;; (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
    ;; (advice-add #'company-tabnine :around #'my-company-tabnine)
    ;; (defun company//sort-by-tabnine (candidates)
    ;;   (if (or (functionp company-backend)
    ;;           (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
    ;;       candidates
    ;;     (let ((candidates-table (make-hash-table :test #'equal))
    ;;           candidates-1
    ;;           candidates-2)
    ;;       (dolist (candidate candidates)
    ;;         (if (eq (get-text-property 0 'company-backend candidate)
    ;;                 'company-tabnine)
    ;;             (unless (gethash candidate candidates-table)
    ;;               (push candidate candidates-2))
    ;;           (push candidate candidates-1)
    ;;           (puthash candidate t candidates-table)))
    ;;       (setq candidates-1 (nreverse candidates-1))
    ;;       (setq candidates-2 (nreverse candidates-2))
    ;;       (nconc (seq-take candidates-1 2)
    ;;              (seq-take candidates-2 2)
    ;;              (seq-drop candidates-1 2)
    ;;              (seq-drop candidates-2 2)))))
    ;; (add-to-list 'company-transformers 'company//sort-by-tabnine t)
    (global-company-mode)
    )
  ;; 改变evil-insert-mode光标形状
  (setq-default evil-insert-state-cursor '("green" (bar . 2)))
  ;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;;      Show key bind for currently entered incomplete command
  ;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  (use-package which-key
    :config
    (progn
      (which-key-mode)
      (which-key-setup-side-window-bottom)))
  ;;bm 管理书签
  (use-package bm
    :ensure t
    :demand t
    :init
    ;; restore on load (even before you require bm)
    (setq bm-restore-repository-on-load t)


    :config
    ;; Allow cross-buffer 'next'
    (setq bm-cycle-all-buffers t)

    ;; where to store persistant files
    (setq bm-repository-file "~/.emacs.d/bm-repository")

    ;; save bookmarks
    (setq-default bm-buffer-persistence t)

    ;; Loading the repository from file when on start up.
    (add-hook 'after-init-hook 'bm-repository-load)

    ;; Saving bookmarks
    (add-hook 'kill-buffer-hook #'bm-buffer-save)

    ;; Saving the repository to file when on exit.
    ;; kill-buffer-hook is not called when Emacs is killed, so we
    ;; must save all bookmarks first.
    (add-hook 'kill-emacs-hook #'(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))

    ;; The `after-save-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state.
    (add-hook 'after-save-hook #'bm-buffer-save)

    ;; Restoring bookmarks
    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)

    ;; The `after-revert-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state. This hook might cause trouble when using packages
    ;; that automatically reverts the buffer (like vc after a check-in).
    ;; This can easily be avoided if the package provides a hook that is
    ;; called before the buffer is reverted (like `vc-before-checkin-hook').
    ;; Then new bookmarks can be saved before the buffer is reverted.
    ;; Make sure bookmarks is saved before check-in (and revert-buffer)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
    :bind (("<f2>" . bm-next)
           ("S-<f2>" . bm-previous)
           ("C-<f2>" . bm-toggle))
    )
  ;;pdf-tools
;;; pdf-tools package and reinstall both as at the start.
  ;; (use-package pdf-tools
  ;;   :ensure t
  ;;   :config
  ;;   (custom-set-variables
  ;;    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  ;;   (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
  ;; (pdf-tools-install)
  ;;ace-window
  (global-set-key (kbd "M-o") 'ace-window)
  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
	  (?m aw-swap-window "Swap Windows")
	  (?M aw-move-window "Move Window")
	  (?c aw-copy-window "Copy Window")
	  (?j aw-switch-buffer-in-window "Select Buffer")
	  (?n aw-flip-window)
	  (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	  (?c aw-split-window-fair "Split Fair Window")
	  (?v aw-split-window-vert "Split Vert Window")
	  (?b aw-split-window-horz "Split Horz Window")
	  (?o delete-other-windows "Delete Other Windows")
	  (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.")
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  ;;hydra
;;;yank
  (defhydra hydra-yank-pop ()
    "yank"
    ("C-y" yank nil)
    ("M-y" yank-pop nil)
    ("y" (yank-pop 1) "next")
    ("Y" (yank-pop -1) "prev")
    ("l" helm-show-kill-ring "list" :color blue)) ; or browse-kill-ring
  (global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
  (global-set-key (kbd "C-y") #'hydra-yank-pop/yank)

  ;;movement
  (global-set-key
   (kbd "C-n")
   (defhydra hydra-move
     (:body-pre (next-line))
     "move"
     ("n" next-line)
     ("p" previous-line)
     ("f" forward-char)
     ("b" backward-char)
     ("a" beginning-of-line)
     ("e" move-end-of-line)
     ("v" scroll-up-command)
     ;; Converting M-v to V here by analogy.
     ("V" scroll-down-command)
     ("l" recenter-top-bottom)))

  ;;page navigation
  (defhydra hydra-page (ctl-x-map "" :pre (widen))
    "page"
    ("]" forward-page "next")
    ("[" backward-page "prev")
    ("n" narrow-to-page "narrow" :bind nil :exit t))
  ;;goto line
  (defhydra hydra-goto-line (goto-map ""
                                      :pre (linum-mode 1)
                                      :post (linum-mode -1))
    "goto-line"
    ("g" goto-line "go")
    ("m" set-mark-command "mark" :bind nil)
    ("q" nil "quit"))
  (global-set-key (kbd "M-g C-m") #'hydra-goto-line/set-mark-command)
  ;;Move Text
  (defhydra hydra-move-text ()
    "Move text"
    ("u" move-text-up "up")
    ("d" move-text-down "down"))

  ;;find file with xf
  (defun x-hydra-pre ()
    (insert "x")
    (let ((timer (timer-create)))
      (timer-set-time timer (timer-relative-time (current-time) 0.5))
      (timer-set-function timer 'hydra-keyboard-quit)
      (timer-activate timer)))

  (defhydra x-hydra (:body-pre x-hydra-pre
                               :color blue
                               :hint nil)
    ("f" (progn (zap-to-char -1 ?x) (ido-find-file))))

  (global-set-key "x" #'x-hydra/body)

  ;;outline minor mode
  (defhydra hydra-outline (:color pink :hint nil)
    "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
    ;; Hide
    ("q" hide-sublevels)      ; Hide everything but the top-level headings
    ("t" hide-body)           ; Hide everything but headings (all body lines)
    ("o" hide-other)          ; Hide other branches
    ("c" hide-entry)          ; Hide this entry's body
    ("l" hide-leaves)         ; Hide body lines in this entry and sub-entries
    ("d" hide-subtree)        ; Hide everything in this entry and sub-entries
    ;; Show
    ("a" show-all)            ; Show (expand) everything
    ("e" show-entry)          ; Show this heading's body
    ("i" show-children)       ; Show this heading's immediate child sub-headings
    ("k" show-branches)       ; Show all sub-headings under this heading
    ("s" show-subtree)        ; Show (expand) everything in this heading & below
    ;; Move
    ("u" outline-up-heading)               ; Up
    ("n" outline-next-visible-heading)     ; Next
    ("p" outline-previous-visible-heading) ; Previous
    ("f" outline-forward-same-level)       ; Forward - same level
    ("b" outline-backward-same-level)      ; Backward - same level
    ("z" nil "leave"))
  (global-set-key (kbd "C-c #") 'hydra-outline/body) ; by example
                                        ;goto
  (defhydra goto (:color blue :hint nil)
    "
Goto:
^Char^              ^Word^                ^org^                    ^search^
^^^^^^^^---------------------------------------------------------------------------
_c_: 2 chars        _w_: word by char     _h_: headline in buffer  _o_: helm-occur
_C_: char           _W_: some word        _a_: heading in agenda   _p_: helm-swiper
_L_: char in line   _s_: subword by char  _q_: swoop org buffers   _f_: search forward
^  ^                _S_: some subword     ^ ^                      _b_: search backward
-----------------------------------------------------------------------------------
_B_: helm-buffers       _l_: avy-goto-line
_m_: helm-mini          _i_: ace-window
_R_: helm-recentf

_n_: Navigate           _._: mark position _/_: jump to mark
"
    ("c" avy-goto-char-2)
    ("C" avy-goto-char)
    ("L" avy-goto-char-in-line)
    ("w" avy-goto-word-1)
    ;; jump to beginning of some word
    ("W" avy-goto-word-0)
    ;; jump to subword starting with a char
    ("s" avy-goto-subword-1)
    ;; jump to some subword
    ("S" avy-goto-subword-0)

    ("l" avy-goto-line)
    ("i" ace-window)

    ("h" helm-org-headlines)
    ("a" helm-org-agenda-files-headings)
    ("q" helm-multi-swoop-org)

    ("o" helm-occur)
    ("p" swiper-helm)

    ("f" isearch-forward)
    ("b" isearch-backward)

    ("." org-mark-ring-push :color red)
    ("/" org-mark-ring-goto :color blue)
    ("B" helm-buffers-list)
    ("m" helm-mini)
    ("R" helm-recentf)
    ("n" hydra-navigate/body))

  (global-set-key (kbd "s-g") 'goto/body)
  ;;hydra skan-user-buffer
  (defun my/name-of-buffers (n)
    "Return the names of the first N buffers from `buffer-list'."
    (let ((bns
           (delq nil
                 (mapcar
                  (lambda (b)
                    (unless (string-match "^ " (setq b (buffer-name b)))
                      b))
                  (buffer-list)))))
      (subseq bns 1 (min (1+ n) (length bns)))))

  ;; Given ("a", "b", "c"), return "1. a, 2. b, 3. c".
  (defun my/number-names (list)
    "Enumerate and concatenate LIST."
    (let ((i 0))
      (mapconcat
       (lambda (x)
         (format "%d. %s" (cl-incf i) x))
       list
       ", ")))

  (defvar my/last-buffers nil)

  (defun my/switch-to-buffer (arg)
    (interactive "p")
    (switch-to-buffer
     (nth (1- arg) my/last-buffers)))

  (defun my/switch-to-buffer-other-window (arg)
    (interactive "p")
    (switch-to-buffer-other-window
     (nth (1- arg) my/last-buffers)))
  (global-set-key
   "\C-o"
   (defhydra my/switch-to-buffer (:exit t
                                        :body-pre (setq my/last-buffers
                                                        (my/name-of-buffers 4)))
     "
other buffers: %s(my/number-names my/last-buffers)

"
     ("o" (my/switch-to-buffer 1))
     ("1" (my/switch-to-buffer 1))
     ("2" (my/switch-to-buffer 2))
     ("3" (my/switch-to-buffer 3))
     ("4" (my/switch-to-buffer 4))
     ("<escape>" nil)))

  ;;flycheck
  (global-flycheck-mode)
  ;;spaceline-all-the-icons
  ;; (use-package spaceline-all-the-icons 
  ;;   :after spaceline
  ;;   :config (spaceline-all-the-icons-theme)
  ;;   (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  ;;   ;(spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  ;;   ;(spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
  ;;   ;(spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
  ;;   ;(spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
  ;;   )
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
  (evil-leader/set-key
    "ff" 'find-file
    "fs" 'save-buffer
    "j" 'pyim-convert-string-at-point
    "fr" 'counsel-recentf
    "s"  'swiper
    "cmr" 'counsel-mark-ring
    "gl"  'avy-goto-line
    "yd" 'youdao-dictionary-search
    "gw0"  'avy-goto-word-0
    "gw1"  'avy-goto-word-1
                                        ;"ob" 'org-bold ;org-bold命令有错误
    "m"  'hydra-goto-line/set-mark-command
    "bb" 'switch-to-buffer
    "0"  'select-window-0
    "1"  'select-window-1
    "2"  'select-window-2
    "3"  'select-window-3
    "w/" 'split-window-right
    "w-" 'split-window-below
                                        ;";"  'counsel-M-x
    "wM" 'delete-other-windows
    )
  ;; ivy
  (use-package ivy
    :ensure t
    :defer t
    :diminish ivy-mode
    :config
    (progn
      (ivy-mode)
      (with-eval-after-load 'recentf
        (setq ivy-use-virtual-buffers t))
      (setq ivy-height 12
            ivy-do-completion-in-region nil
            ivy-wrap t
            ivy-extra-directories nil
            ivy-fixed-height-minibuffer t
            ;; Don't use ^ as initial input
            ivy-initial-inputs-alist nil
            ;; highlight til EOL
            ivy-format-function #'ivy-format-function-line
            ;; disable magic slash on non-match
            ;; ~ to /home/user
            ;; ivy-magic-tilde nil
            ivy-magic-slash-non-match-action nil)
      ;; (setq ivy-re-builders-alist
      ;;       '((t . ivy--regex-fuzzy)))
      ;; (setq confirm-nonexistent-file-or-buffer t)
      (setq ivy-re-builders-alist
            '((t   . ivy--regex-ignore-order)))
      (evil-make-overriding-map ivy-occur-mode-map 'normal)
      ))
  ;; dired-mode
  ;;返回上层目录，我绑定了快捷键i, 特别好按，非常流畅
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "i")
                (lambda () (interactive) (find-alternate-file "..")))))

  ;;Chinese and English fonts alignment
  (use-package cnfonts
    :config
    (cnfonts-enable)
    (setq cnfonts-use-face-font-rescale t)
    )
  ;; org
  (with-eval-after-load 'org
    (defun my-org-config ()
      (make-variable-buffer-local 'company-backends)
      (add-to-list 'company-backends
                   '(company-yasnippet company-tabnine company-dabbrev-code :separate))
      )
    (add-hook 'org-mode-hook #'my-org-config)
    )
  ;; poly-R
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
;;;  ESS (Emacs Speaks Statistics)
  (with-eval-after-load 'ess
    (ess-set-style 'RStudio)
    (setq ess-offset-arguments 'prev-line)
    ;;一定要记得加company-yasnipet
    (defun my-ess-config ()
      (make-variable-buffer-local 'company-backends)
      (add-to-list 'company-backends
                   '(company-R-args company-yasnippet company-tabnine company-R-library  company-R-objects company-dabbrev-code :separate))
      )
    (add-hook 'ess-mode-hook #'my-ess-config)
    ;; (add-hook 'find-file-hook 'my-r-style-hook)
    ;; (defun my-r-style-hook ()
    ;;   (when (string-match (file-name-extension buffer-file-name) "[r|R]$")
    ;;     (ess-set-style 'RStudio)
    ;;     )
    ;;   )
    ;; ;;highlight color code
    (add-hook 'ess-mode-hook
              '(lambda()
                 (font-lock-add-keywords
                  nil
                  '(                  ("\\<\\(if\\|for\\|function\\|return\\|$\\|@\\)\\>[\n[:blank:]]*(" 1
                                       font-lock-keyword-face) ; must go first to override highlighting below
                                      ("\\<\\([.A-Za-z][._A-Za-z0-9]*\\)[\n[:blank:]]*(" 1
                                       font-lock-function-name-face) ; highlight function names
                                      ("\\([(,]\\|[\n[:blank:]]*\\)\\([.A-Za-z][._A-Za-z0-9]*\\)[\n[:blank:]]*=[^=]"
                                       2 font-lock-reference-face)
                                      ;; highlight numbers
                                      ("\\(-?[0-9]*\\.?[0-9]*[eE]?-?[0-9]+[iL]?\\)" 1 font-lock-type-face)
                                      ;; highlight operators
                                      ("\\(\\$\\|\\@\\|\\!\\|\\%\\|\\^\\|\\&\\|\\*\\|\(\\|\)\\|\{\\|\}\\|\\[\\|\\]\\|\\-\\|\\+\\|\=\\|\\/\\|\<\\|\>\\|:\\)" 1 font-lock-builtin-face)
                                      highlight S4 methods
                                      ("\\(setMethod\\|setGeneric\\|setGroupGeneric\\|setClass\\|setRefClass\\|setReplaceMethod\\)" 1 font-lock-reference-face)
                                      ;; highlight packages called through ::, :::
                                      ("\\(\\w+\\):\\{2,3\\}" 1 font-lock-constant-face)
                                      ))
                 ))
    )
  ;;pipe function %>% key
  (defun then_R_operator ()
    "R - %>% operator or 'then' pipe operator"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
                                        ;(reindent-then-newline-and-indent)
    (just-one-space 1))
  (define-key ess-mode-map (kbd "C-c C-m") 'then_R_operator)
  (define-key inferior-ess-mode-map (kbd "C-c C-m") 'then_R_operator)
  ;;pipe function <- key
  (defun assign_R_operator ()
    "R - <- operator or 'assign' operator"
    (interactive)
    (just-one-space 1)
    (insert "<-")
    (just-one-space 1)
                                        ;(reindent-then-newline-and-indent)
    )
  (define-key ess-mode-map (kbd "C-c C--") 'assign_R_operator)
  (define-key inferior-ess-mode-map (kbd "M--") 'assign_R_operator)
  ;;bracket function () key
  (defun bracket_R_operator ()
    "R - () operator or 'bracket' operator"
    (interactive)
                                        ; (just-one-space 1)
    (insert "()")
    (backward-char 1)
                                        ;(reindent-then-newline-and-indent)
    )
  (define-key ess-mode-map (kbd "C-c C-b") 'bracket_R_operator)
  (define-key inferior-ess-mode-map (kbd "C-c C-b") 'bracket_R_operator)
  ;; ;;indent-region-as-r
  ;; (defun ess-indent-region-as-r (beg end)
  ;;   "Format region of code R using the R parser."
  ;;   (interactive "r")
  ;;   (let ((string (replace-regexp-in-string
  ;;                  "\"" "\\\\\\&"
  ;;                  (replace-regexp-in-string ;; how to avoid this double matching?
  ;;                   "\\\\\"" "\\\\\\&" (buffer-substring-no-properties beg end))))
  ;;         (buf (get-buffer-create "*ess-command-output*")))
  ;;     (ess-force-buffer-current "Process to load into:")
  ;;     (ess-command
  ;;      (format
  ;;       "local({oo <- options(keep.source = FALSE);
  ;; cat('\n', paste(deparse(parse(text = \"%s\")[[1L]]), collapse = '\n'), '\n', sep = '')
  ;; options(oo)})\n"
  ;;       string) buf)
  ;;     (with-current-buffer buf
  ;;       (goto-char (point-max))
  ;;       ;; (skip-chars-backward "\n")
  ;;       (let ((end (point)))
  ;;         (goto-char (point-min))
  ;;         (goto-char (1+ (point-at-eol)))
  ;;         (setq string (buffer-substring-no-properties (point) end))
  ;;         ))
  ;;     (delete-region beg end)
  ;;     (insert string)
  ;;     ))
  ;;from iqss.github.io
  ;; (add-hook 'ess-r-mode-hook
  ;;            (lambda()
  ;;              'eglot-ensure
  ;;              (make-local-variable 'company-backends)
  ;;              (delete-dups (push 'company-capf company-backends))
  ;;             (delete-dups (push 'company-files company-backends))))
  ;; Set ESS options
  ;; (setq
  ;;  ess-auto-width 'window
  ;;  ess-use-auto-complete nil
  ;;  ess-use-company 't
  ;;  ;; ess-r-package-auto-set-evaluation-env nil
  ;;  inferior-ess-same-window nil
  ;;  ess-indent-with-fancy-comments nil   ; don't indent comments
  ;;  ess-eval-visibly t                   ; enable echoing input
  ;;  ess-eval-empty t                     ; don't skip non-code lines.
  ;;  ess-ask-for-ess-directory nil        ; start R in the working directory by default
  ;;  ess-ask-for-ess-directory nil        ; start R in the working directory by default
  ;;  ess-R-font-lock-keywords             ; font-lock, but not too much
  ;;  (quote
  ;;   ((ess-R-fl-keyword:modifiers)
  ;;    (ess-R-fl-keyword:fun-defs . t)
  ;;    (ess-R-fl-keyword:keywords . t)
  ;;    (ess-R-fl-keyword:assign-ops  . t)
  ;;    (ess-R-fl-keyword:constants . 1)
  ;;    (ess-fl-keyword:fun-calls . t)
  ;;    (ess-fl-keyword:numbers)
  ;;    (ess-fl-keyword:operators . t)
  ;;    (ess-fl-keyword:delimiters)
  ;;    (ess-fl-keyword:=)
  ;;    (ess-R-fl-keyword:F&T))))
  ;; ;;formatR
  ;; From Walmes Zeviani
  ;; https://github.com/basille/.emacs.d/issues/1
  (defun ess-indent-region-with-formatr (beg end)
    "Format region of code R using formatR::tidy_source()."
    (interactive "r")
    (let ((string
           (replace-regexp-in-string
            "\"" "\\\\\\&"
            (replace-regexp-in-string ;; how to avoid this double matching?
             "\\\\\"" "\\\\\\&"
             (buffer-substring-no-properties beg end))))
	      (buf (get-buffer-create "*ess-command-output*")))
      (ess-force-buffer-current "Process to load into:")
      (ess-command
       (format                          ; R parser use 'width.cutoff = 60L'
        "local({formatR::tidy_source(text = \"\n%s\", arrow = TRUE, width.cutoff = 60L) })\n"
        string) buf)
      (with-current-buffer buf
        (goto-char (point-max))
        ;; (skip-chars-backward "\n")
        (let ((end (point)))
	      (goto-char (point-min))
	      (goto-char (1+ (point-at-eol)))
	      (setq string (buffer-substring-no-properties (point) end))
	      ))
      (delete-region beg end)
      (insert string)
      (delete-char -1)
      ))
  (defun ess-indent-region-with-formatR-tidy-source (beg end)
    "Format region of code R using formatR::tidy_source()."
    (interactive "r")
    (let ((string
           (replace-regexp-in-string
            "\"" "\\\\\\&"
            (replace-regexp-in-string ;; how to avoid this double matching?
             "\\\\\"" "\\\\\\&"
             (buffer-substring-no-properties beg end))))
          (buf (get-buffer-create "*ess-command-output*")))
      (ess-force-buffer-current "Process to load into:")
      (ess-command
       (format
        "local({
          formatR::tidy_source(text=\"\n%s\",
                               arrow=TRUE, width.cutoff=60) })\n"
        string) buf)
      (with-current-buffer buf
        (goto-char (point-max))
        ;; (skip-chars-backward "\n")
        (let ((end (point)))
          (goto-char (point-min))
          (goto-char (1+ (point-at-eol)))
          (setq string (buffer-substring-no-properties (point) end))
          ))
      (delete-region beg end)
      (insert string)
      (delete-backward-char 2)
      ))
  ;;styler
  (defun ess-indent-region-with-styler (beg end)
    "Format region of code R using styler::style_text()."
    (interactive "r")
    (let ((string
           (replace-regexp-in-string
            "\"" "\\\\\\&"
            (replace-regexp-in-string ;; how to avoid this double matching?
             "\\\\\"" "\\\\\\&"
             (buffer-substring-no-properties beg end))))
	      (buf (get-buffer-create "*ess-command-output*")))
      (ess-force-buffer-current "Process to load into:")
      (ess-command
       (format
        "local({options(styler.colored_print.vertical = FALSE);styler::style_text(text = \"\n%s\", reindention = styler::specify_reindention(regex_pattern = \"###\", indention = 0), indent_by = 4)})\n"
        string) buf)
      (with-current-buffer buf
        (goto-char (point-max))
        ;; (skip-chars-backward "\n")
        (let ((end (point)))
	      (goto-char (point-min))
	      (goto-char (1+ (point-at-eol)))
	      (setq string (buffer-substring-no-properties (point) end))
	      ))
      (delete-region beg end)
      (insert string)
      (delete-char -1)
      ))

  ;; 以下是zilongshanren配置 
  ;; ;;解决org表格里面中英文对齐的问题 
  ;; (when (configuration-layer/layer-usedp 'chinese)
  ;;   (when (and (spacemacs/system-is-mac) window-system)
  ;;     (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))


  ;; Setting Chinese Font
  (when (and (spacemacs/system-is-mswindows) window-system)
    (setq ispell-program-name "aspell")
    (setq w32-pass-alt-to-system nil)
    (setq w32-apps-modifier 'super)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "Microsoft Yahei" :size 14))))

  (fset 'evil-visual-update-x-selection 'ignore)

  ;; force horizontal split window
  (setq split-width-threshold 120)
  ;; (linum-relative-on)

  (spacemacs|add-company-backends :modes text-mode)

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

  (defun js-indent-line ()
    "Indent the current line as JavaScript."
    (interactive)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (point) (save-excursion (back-to-indentation) (point)))))
      (if (nth 3 parse-status)
          'noindent
        (indent-line-to (js--proper-indentation parse-status))
        (when (> offset 0) (forward-char offset)))))

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
  (spacemacs/set-leader-keys "otm" 'zilongshanren/toggle-major-mode)

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
  (when (spacemacs/system-is-mswindows)
    (progn (setq find-file-hook nil)
           (setq vc-handled-backends nil)
           (setq magit-refresh-status-buffer nil)
           (add-hook 'find-file-hook 'spacemacs/check-large-file)

           ;; emax.7z in not under pdumper release
           ;; https://github.com/m-parashar/emax64/releases/tag/pdumper-20180619
           (defvar emax-root (concat (expand-file-name "~") "/emax"))

           (when (file-exists-p emax-root)
             (progn
               (defvar emax-root (concat (expand-file-name "~") "/emax"))
               (defvar emax-bin64 (concat emax-root "/bin64"))
               (defvar emax-mingw64 (concat emax-root "/mingw64/bin"))
               (defvar emax-lisp (concat emax-root "/lisp"))

               (setq exec-path (cons emax-bin64 exec-path))
               (setenv "PATH" (concat emax-bin64 ";" (getenv "PATH")))

               (setq exec-path (cons emax-mingw64 exec-path))
               (setenv "PATH" (concat emax-mingw64 ";" (getenv "PATH")))
               ))

           (add-hook 'projectile-mode-hook '(lambda () (remove-hook 'find-file-hook #'projectile-find-file-hook-function)))))

  (setq exec-path (cons "/Users/lionqu/.nvm/versions/node/v10.16.0/bin/" exec-path))
  (setenv "PATH" (concat "/Users/lionqu/.nvm/versions/node/v10.16.0/bin:" (getenv "PATH")))

  (defun counsel-locate-cmd-es (input)
    "Return a shell command based on INPUT."
    (counsel-require-program "es.exe")
    (encode-coding-string (format "es.exe -i -r -p %s"
                                  (counsel-unquote-regex-parens
                                   (ivy--regex input t)))
                          'gbk))
  ;; (add-hook 'text-mode-hook 'spacemacs/toggle-spelling-checking-on)

  (add-hook 'org-mode-hook 'emojify-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)

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
