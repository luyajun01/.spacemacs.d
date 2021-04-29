;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Some usefule Utilities.
;;

;;; Code:

(require 'init-const)

;; Display available keybindings in popup
(use-package which-key
  :diminish
  ;; :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-show-remaining-keys t)
  :config
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c c" "counsel")
  (which-key-add-key-based-replacements "C-c n" "org-roam")
  (which-key-add-key-based-replacements "C-c t" "hl-todo")
  (which-key-add-key-based-replacements "C-c v" "ivy-view")
  (which-key-add-key-based-replacements "C-c C-z" "browse")

  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x @" "modifior")
  (which-key-add-key-based-replacements "C-x X" "edebug")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x t" "tab")
  (which-key-add-key-based-replacements "C-x C-a" "edebug")


  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c ," "overseer")
  (which-key-add-major-mode-key-based-replacements 'python-mode
    "C-c C-t" "python-skeleton")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-x" "markdown-toggle")

  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-x" "markdown-toggle"))

;; Persistent the scratch buffer
;; (use-package persistent-scratch
;;   :diminish
;;   :bind (:map persistent-scratch-mode-map
;;          ([remap kill-buffer] . (lambda (&rest _)
;;                                   (interactive)
;;                                   (user-error "Scrach buffer cannot be killed")))
;;          ([remap revert-buffer] . persistent-scratch-restore)
;;          ([remap revert-this-buffer] . persistent-scratch-restore))
;;   :hook ((after-init . persistent-scratch-autosave-mode)
;;          (lisp-interaction-mode . persistent-scratch-mode)))

;; Search tools
;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Fast search tool `ripgrep'
(use-package rg
  :defines projectile-command-map
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu)
         :map rg-mode-map
         ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep #'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))

  (with-eval-after-load 'counsel
    (bind-keys
     :map rg-global-map
     ("R" . counsel-rg)
     ("F" . counsel-fzf))))

;; Dictionary
;; (when sys/macp
;;   (use-package osx-dictionary
;;     :bind (("C-c D" . osx-dictionary-search-pointer))))

;; Youdao Dictionary
(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :bind (("C-c y" . my-youdao-dictionary-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point)
         :map youdao-dictionary-mode-map
         ("h" . youdao-dictionary-hydra/body)
         ("?" . youdao-dictionary-hydra/body))
  :init
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词

  (defun my-youdao-dictionary-search-at-point ()
    "Search word at point and display result with `posframe', `pos-tip', or buffer."
    (interactive)
    (if (display-graphic-p)
        (if emacs/>=26p
            (youdao-dictionary-search-at-point-posframe)
          (youdao-dictionary-search-at-point-tooltip))
      (youdao-dictionary-search-at-point)))
  :config
  (with-eval-after-load 'hydra
    (defhydra youdao-dictionary-hydra (:color blue)
      ("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
      ("y" youdao-dictionary-play-voice-at-point "play voice at point")
      ("q" quit-window "quit")
      ("C-g" nil nil)
      ("h" nil nil)
      ("?" nil nil))))

;; A Simple and cool pomodoro timer
;; (use-package pomidor
;;   :bind ("s-<f12>" . pomidor)
;;   :init
;;   (setq alert-default-style 'mode-line)

;;   (with-eval-after-load 'all-the-icons
;;     (setq alert-severity-faces
;;           '((urgent   . all-the-icons-red)
;;             (high     . all-the-icons-orange)
;;             (moderate . all-the-icons-yellow)
;;             (normal   . all-the-icons-green)
;;             (low      . all-the-icons-blue)
;;             (trivial  . all-the-icons-purple))
;;           alert-severity-colors
;;           `((urgent   . ,(face-foreground 'all-the-icons-red))
;;             (high     . ,(face-foreground 'all-the-icons-orange))
;;             (moderate . ,(face-foreground 'all-the-icons-yellow))
;;             (normal   . ,(face-foreground 'all-the-icons-green))
;;             (low      . ,(face-foreground 'all-the-icons-blue))
;;             (trivial  . ,(face-foreground 'all-the-icons-purple)))))

;;   (when sys/macp
;;     (setq pomidor-play-sound-file
;;           (lambda (file)
;;             (when (executable-find "afplay")
;;               (start-process "pomidor-play-sound" nil "afplay" file))))))

;; Nice writing
(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.618))

;; Edit text for browsers with GhostText or AtomicChrome extension
;; (use-package atomic-chrome
;;   :hook ((emacs-startup . atomic-chrome-start-server)
;;          (atomic-chrome-edit-mode . (lambda ()
;;                                       "Enter edit mode and delete other windows."
;;                                       (and (fboundp 'olivetti-mode)
;;                                            (olivetti-mode 1))
;;                                       (delete-other-windows))))
;;   :init (setq atomic-chrome-buffer-open-style 'frame)
;;   :config
;;   (if (fboundp 'gfm-mode)
;;       (setq atomic-chrome-url-major-mode-alist
;;             '(("github\\.com" . gfm-mode)))))

;; Music player
;; (use-package bongo
;;   :bind ("C-<f9>" . bongo)
;;   :config
;;   (with-eval-after-load 'dired
;;     (with-no-warnings
;;       (defun bongo-add-dired-files ()
;;         "Add marked files to the Bongo library."
;;         (interactive)
;;         (bongo-buffer)
;;         (let (file (files nil))
;;           (dired-map-over-marks
;;            (setq file (dired-get-filename)
;;                  files (append files (list file)))
;;            nil t)
;;           (with-bongo-library-buffer
;;            (mapc 'bongo-insert-file files)))
;;         (bongo-switch-buffers))
;;       (bind-key "b" #'bongo-add-dired-files dired-mode-map))))

;; IRC
(use-package erc
  :ensure nil
  :defines erc-autojoin-channels-alist
  :init (setq erc-rename-buffers t
              erc-interpret-mirc-color t
              erc-lurker-hide-list '("JOIN" "PART" "QUIT")
              erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))

;; A stackoverflow and its sisters' sites reader
(when emacs/>=26p
  ;; (use-package howdoyou
  ;;   :bind (:map howdoyou-mode-map
  ;;          ("q" . kill-buffer-and-window))
  ;;   :hook (howdoyou-mode . read-only-mode))
  )

;; text mode directory tree
;; (use-package ztree
;;   :custom-face
;;   (ztreep-header-face ((t (:inherit diff-header))))
;;   (ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
;;   (ztreep-leaf-face ((t (:inherit diff-index))))
;;   (ztreep-node-face ((t (:inherit font-lock-variable-name-face))))
;;   (ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
;;   (ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
;;   (ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
;;   (ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
;;   (ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
;;   (ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
;;   (ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
;;   :pretty-hydra
;;   ((:title (pretty-hydra-title "Ztree" 'octicon "diff" :height 1.2 :v-adjust 0)
;;     :color pink :quit-key "q")
;;    ("Diff"
;;     (("C" ztree-diff-copy "copy" :exit t)
;;      ("h" ztree-diff-toggle-show-equal-files "show/hide equals" :exit t)
;;      ("H" ztree-diff-toggle-show-filtered-files "show/hide ignores" :exit t)
;;      ("D" ztree-diff-delete-file "delete" :exit t)
;;      ("v" ztree-diff-view-file "view" :exit t)
;;      ("d" ztree-diff-simple-diff-files "simple diff" :exit t)
;;      ("r" ztree-diff-partial-rescan "partial rescan" :exit t)
;;      ("R" ztree-diff-full-rescan "full rescan" :exit t))
;;     "View"
;;     (("RET" ztree-perform-action "expand/collapse or view" :exit t)
;;      ("SPC" ztree-perform-soft-action "expand/collapse or view in other" :exit t)
;;      ("TAB" ztree-jump-side "jump side" :exit t)
;;      ("g" ztree-refresh-buffer "refresh" :exit t)
;;      ("x" ztree-toggle-expand-subtree "expand/collapse" :exit t)
;;      ("<backspace>" ztree-move-up-in-tree "go to parent" :exit t))))
;;   :bind (:map ztreediff-mode-map
;;          ("C-<f5>" . ztree-hydra/body))
;;   :init (setq ztree-draw-unicode-lines t
;;               ztree-show-number-of-children t))

;; Misc
;; (use-package copyit)                    ; copy path, url, etc.
;; (use-package diffview)                  ; side-by-side diff view
;; (use-package esup)                      ; Emacs startup profiler
;; (use-package focus)                     ; Focus on the current region
;; (use-package list-environment)
;; (use-package memory-usage)
(unless sys/win32p
  ;; (use-package daemons)                 ; system services/daemons
  ;; (use-package tldr)
  )


;; bin-chen
;; -*- coding: utf-8; lexical-binding: t; -*-

;; Please note functions here could be used in ~/.custom.el

(defun local-require (pkg)
  "Require PKG in site-lisp directory."
  (unless (featurep pkg)
    (load (expand-file-name
           (cond
            ((eq pkg 'go-mode-load)
             (format "%s/go-mode/%s" my-site-lisp-dir pkg))
            (t
             (format "%s/%s/%s" my-site-lisp-dir pkg pkg))))
          t t)))

(defun my-ensure (feature)
  "Make sure FEATURE is required."
  (unless (featurep feature)
    (condition-case nil
        (require feature)
      (error nil))))

(defun my-git-root-dir ()
  "Git root directory."
  (locate-dominating-file default-directory ".git"))

(defun my-git-files-in-rev-command (rev level)
  "Return git command line to show files in REV and LEVEL."
  (unless level (setq level 0))
  (concat "git diff-tree --no-commit-id --name-only -r "
          rev
          (make-string level ?^)))

(defun nonempty-lines (str)
  "Split STR into lines."
  (split-string str "[\r\n]+" t))

(defun my-lines-from-command-output (command)
  "Return lines of COMMAND output."
  (let* ((output (string-trim (shell-command-to-string command)))
         (cands (nonempty-lines output)))
    (delq nil (delete-dups cands))))

(defun run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or whole buffer and replace it with cli output."
  (let* ((orig-point (point))
         (b (if (region-active-p) (region-beginning) (point-min)))
         (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e cmd nil t)
    (goto-char orig-point)))

(defun my-use-tags-as-imenu-function-p ()
  "Can use tags file to build imenu function"
  (my-ensure 'counsel-etags)
  (and (locate-dominating-file default-directory "TAGS")
       ;; latest universal ctags has built in parser for javascript/typescript
       (counsel-etags-universal-ctags-p "ctags")
       (memq major-mode '(typescript-mode js-mode javascript-mode))))

;; {{ copied from http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun my-get-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun my-read-lines (file)
  "Return a list of lines of FILE."
  (split-string (my-get-string-from-file file) "\n" t))
;; }}

(defun my-write-to-file (str file)
  "Write STR to FILE."
  (with-temp-buffer
    (insert str)
    (write-file (file-truename file))))

(defun my-write-to-missing-file (str file)
  "Write STR to FILE if it's missing."
  (unless (file-exists-p file)
    (my-write-to-file str file)))

;; Handier way to add modes to auto-mode-alist
(defun my-add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (push (cons pattern mode) auto-mode-alist)))

(defun my-add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (push (cons pattern mode) interpreter-mode-alist )))

(defun my-what-face (&optional position)
  "Show all faces at POSITION."
  (let* ((face (get-text-property (or position (point)) 'face)))
    (unless (keywordp (car-safe face)) (list face))))

;; String utilities missing from core emacs
(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let* ((pattern (concat "^" (file-name-as-directory directory))))
    (if (string-match-p pattern file) file)))

(defun my-prepare-candidate-fit-into-screen (s)
  (let* ((w (frame-width))
         ;; display kill ring item in one line
         (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
    ;; strip the whitespace
    (setq key (replace-regexp-in-string "^[ \t]+" "" key))
    ;; fit to the minibuffer width
    (if (> (length key) w)
        (setq key (concat (substring key 0 (- w 4)) "...")))
    (cons key s)))

(defun my-select-from-kill-ring (fn)
  "If N > 1, yank the Nth item in `kill-ring'.
If N is nil, use `ivy-mode' to browse `kill-ring'."
  (interactive "P")
  (let* ((candidates (cl-remove-if
                       (lambda (s)
                         (or (< (length s) 5)
                             (string-match-p "\\`[\n[:blank:]]+\\'" s)))
                       (delete-dups kill-ring)))
          (ivy-height (/ (frame-height) 2)))
     (ivy-read "Browse `kill-ring':"
               (mapcar #'my-prepare-candidate-fit-into-screen candidates)
               :action fn)))

(defun my-delete-selected-region ()
  "Delete selected region."
  (when (region-active-p)
    (delete-region (region-beginning) (region-end))))

(defun my-insert-str (str)
  "Insert STR into current buffer."
  ;; ivy8 or ivy9
  (if (consp str) (setq str (cdr str)))
  ;; evil-mode?
  (if (and (functionp 'evil-normal-state-p)
           (boundp 'evil-move-cursor-back)
           (evil-normal-state-p)
           (not (eolp))
           (not (eobp)))
      (forward-char))

  (my-delete-selected-region)

  ;; insert now
  (insert str)
  str)

(defun my-line-str (&optional line-end)
  (buffer-substring-no-properties (line-beginning-position)
                                  (if line-end line-end (line-end-position))))

(defun my-is-in-one-line (b e)
  (save-excursion
    (goto-char b)
    (and (<= (line-beginning-position) b)
         (<= e (line-end-position)))))

(defun my-buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun my-selected-str ()
  "Get string of selected region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun my-use-selected-string-or-ask (&optional hint)
  "Use selected region or ask for input.
If HINT is empty, use symbol at point."
  (cond
   ((region-active-p)
    (my-selected-str))
   ((or (not hint) (string= "" hint))
    (thing-at-point 'symbol))
   (t
    (read-string hint))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defvar my-load-user-customized-major-mode-hook t)

(defun buffer-too-big-p ()
  "Test if current buffer is too big."
  ;; 5000 lines
  (> (buffer-size) (* 5000 80)))

(defun my-file-too-big-p (file)
  "Test if FILE is too big."
  (> (nth 7 (file-attributes file))
     (* 5000 64)))

(defvar my-force-buffer-file-temp-p nil)
(defun is-buffer-file-temp ()
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file."
  (interactive)
  (let* ((f (buffer-file-name)) (rlt t))
    (cond
     ((not my-load-user-customized-major-mode-hook)
      (setq rlt t))

     ((and (buffer-name) (string-match "\\* Org SRc" (buffer-name)))
      ;; org-babel edit inline code block need calling hook
      (setq rlt nil))

     ((null f)
      (setq rlt t))

     ((string-match (concat "^" temporary-file-directory) f)
      ;; file is create from temp directory
      (setq rlt t))

     ((and (string-match "\.html$" f)
           (file-exists-p (replace-regexp-in-string "\.html$" ".org" f)))
      ;; file is a html file exported from org-mode
      (setq rlt t))

     (my-force-buffer-file-temp-p
      (setq rlt t))

     (t
      (setq rlt nil)))
    rlt))

(defvar my-mplayer-extra-opts ""
  "Extra options for mplayer (ao or vo setup).
For example, you can '(setq my-mplayer-extra-opts \"-fs -ao alsa -vo vdpau\")'.")

(defun my-guess-mplayer-path ()
  "Guess cli program mplayer's path."
  (let* ((program "mplayer")
         (common-opts "-fs -quiet"))
    (cond
     (*is-a-mac*
      (setq program "mplayer"))

     (*linux*
      (setq program "mplayer -stop-xscreensaver"))

     (*cygwin*
      (if (file-executable-p "/cygdrive/c/mplayer/mplayer.exe")
          (setq program "/cygdrive/c/mplayer/mplayer.exe")
        (setq program "/cygdrive/d/mplayer/mplayer.exe")))

     ;; windows
     (t
      (if (file-executable-p "c:\\\\mplayer\\\\mplayer.exe")
          (setq program "c:\\\\mplayer\\\\mplayer.exe")
        (setq program "d:\\\\mplayer\\\\mplayer.exe"))))

    (format "%s %s %s" program common-opts my-mplayer-extra-opts)))

(defun my-guess-image-viewer-path (image &optional stream-p)
  "How to open IMAGE which could be STREAM-P."
  (cond
   (*is-a-mac*
    (format "open %s &" image))

   (*linux*
    (if stream-p (format "curl -L %s | feh -F - &" image)
      (format "feh -F %s &" image)))

   (*cygwin*
    "feh -F")

   (t ; windows
    (format "rundll32.exe %s\\\\System32\\\\\shimgvw.dll, ImageView_Fullscreen %s &"
            (getenv "SystemRoot")
            image))))

(defun my-gclip ()
  "Get clipboard content."
  (let* ((powershell-program (executable-find "powershell.exe")))
    (cond
     ;; Windows
     ((and *win64* (fboundp 'w32-get-clipboard-data))
      ;; `w32-set-clipboard-data' makes `w32-get-clipboard-data' always return null
      (w32-get-clipboard-data))

     ;; Windows 10
     (powershell-program
      (string-trim-right
       (with-output-to-string
         (with-current-buffer standard-output
           (call-process powershell-program nil t nil "-command" "Get-Clipboard")))))

     ;; xclip can handle
     (t
      (xclip-get-selection 'clipboard)))))

(defvar my-ssh-client-user nil
  "User name of ssh client.")

(defun my-pclip (str-val)
  "Put STR-VAL into clipboard."
  (let* ((win64-clip-program (executable-find "clip.exe"))
         ssh-client)
    (cond
     ;; Windows 10 or Windows 7
     ((and win64-clip-program)
      (with-temp-buffer
        (insert str-val)
        (call-process-region (point-min) (point-max) win64-clip-program)))

     ;; Windows
     ((and *win64* (fboundp 'w32-set-clipboard-data))
      ;; Don't know why, but on Windows 7 this API does not work.
      (w32-set-clipboard-data str-val))

     ;; If Emacs is inside an ssh session, place the clipboard content
     ;; into "~/.tmp-clipboard" and send it back into ssh client
     ;; Make sure you already set up ssh correctly.
     ;; Only enabled if ssh server is macOS
     ((and (setq ssh-client (getenv "SSH_CLIENT"))
           (not (string= ssh-client ""))
           *is-a-mac*)
      (let* ((file "~/.tmp-clipboard")
             (ip (car (split-string ssh-client "[ \t]+")))
             (cmd (format "scp %s %s@%s:~/" file my-ssh-client-user ip)))
        (when my-ssh-client-user
          (my-write-to-file str-val file)
          (shell-command cmd)
          ;; clean up
          (delete-file file))))

     ;; xclip can handle
     (t
      (xclip-set-selection 'clipboard str-val)))))
;; }}

(defun my-should-use-minimum-resource ()
  "Some files should use minimum resource (no syntax highlight, no line number display)."
  (and buffer-file-name
       (string-match-p "\.\\(mock\\|min\\|bundle\\)\.js" buffer-file-name)))

(defun my-async-shell-command (command)
  "Execute string COMMAND asynchronously."
  (let* ((proc (start-process "Shell"
                              nil
                              shell-file-name
                              shell-command-switch command)))
    (set-process-sentinel proc `(lambda (process signal)
                                  (let* ((status (process-status process)))
                                    (when (memq status '(exit signal))
                                      (unless (string= (substring signal 0 -1) "finished")
                                        (message "Failed to run \"%s\"." ,command))))))))

;; reply y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; {{ code is copied from https://liu233w.github.io/2016/09/29/org-python-windows.org/

(defun my-setup-language-and-encode (language-name coding-system)
  "Set up LANGUAGE-NAME and CODING-SYSTEM at Windows.
For example,
- \"English\" and 'utf-16-le
- \"Chinese-GBK\" and 'gbk"
  (cond
   ((eq system-type 'windows-nt)
    (set-language-environment language-name)
    (prefer-coding-system 'utf-8)
    (set-terminal-coding-system coding-system)

    (modify-coding-system-alist 'process "*" coding-system)
    (defun my-windows-shell-mode-coding ()
      (set-buffer-file-coding-system coding-system)
      (set-buffer-process-coding-system coding-system coding-system))
    (add-hook 'shell-mode-hook #'my-windows-shell-mode-coding)
    (add-hook 'inferior-python-mode-hook #'my-windows-shell-mode-coding)

    (defun my-org-babel-execute:python-hack (orig-func &rest args)
      ;; @see https://github.com/Liu233w/.spacemacs.d/issues/6
      (let* ((coding-system-for-write 'utf-8))
        (apply orig-func args)))
    (advice-add 'org-babel-execute:python :around #'my-org-babel-execute:python-hack))

   (t
    (set-language-environment "UTF-8")
    (prefer-coding-system 'utf-8))))
;; }}

(defun my-skip-white-space (start step)
  "Skip white spaces from START, return position of first non-space character.
If STEP is 1,  search in forward direction, or else in backward direction."
  (let* ((b start)
         (e (if (> step 0) (line-end-position) (line-beginning-position))))
    (save-excursion
      (goto-char b)
      (while (and (not (eq b e)) (memq (following-char) '(9 32)))
        (forward-char step))
      (point))))

(defun my-comint-current-input-region ()
  "Region of current shell input."
  (cons (process-mark (get-buffer-process (current-buffer)))
        (line-end-position)))

(defun my-comint-kill-current-input ()
  "Kill current input in shell."
  (interactive)
  (let* ((region (my-comint-current-input-region)))
    (kill-region (car region) (cdr region))))

(defun my-comint-current-input ()
  "Get current input in shell."
  (let* ((region (my-comint-current-input-region)))
    (string-trim (buffer-substring-no-properties (car region) (cdr region)))))

(defun my-rescan-imenu-items (&optional index-function)
  "Get imenu items using INDEX-FUNCTION."
  (my-ensure 'imenu)
  (let* ((imenu-auto-rescan t)
         (imenu-create-index-function (or index-function imenu-create-index-function))
         (imenu-auto-rescan-maxout (buffer-size))
         (items (imenu--make-index-alist t)))
    (delete (assoc "*Rescan*" items) items)))

(defun my-create-range (&optional inclusive)
  "Return range by font face.
Copied from 3rd party package evil-textobj."
  (let* ((point-face (my-what-face))
         (pos (point))
         (backward-point pos) ; last char when stop, including white space
         (backward-none-space-point pos) ; last none white space char
         (forward-point pos) ; last char when stop, including white space
         (forward-none-space-point pos) ; last none white space char
         (start pos)
         (end pos))

    ;; check chars backward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (>= (- (point) 1) (point-min)))
          (backward-char)
          (if (= 32 (char-after))
              (setq backward-point (point))
            (if (equal point-face (my-what-face))
                (progn (setq backward-point (point))
                       (setq backward-none-space-point (point)))
              (setq continue nil))))))

    ;; check chars forward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (< (+ (point) 1) (point-max)))
          (forward-char)
          (let ((forward-point-face (my-what-face)))
            (if (= 32 (char-after))
                (setq forward-point (point))
              (if (equal point-face forward-point-face)
                  (progn (setq forward-point (point))
                         (setq forward-none-space-point (point)))
                (setq continue nil)))))))

    (cond
     (inclusive
      (setq start backward-none-space-point)
      (setq end forward-none-space-point))
     (t
      (setq start (1+ backward-none-space-point))
      (setq end (1- forward-none-space-point))))

    (cons start (1+ end))))

(defun my-get-char (position)
  "Get character at POSITION."
  (save-excursion
    (goto-char position)
    (following-char)))

(defun my-pinyinlib-build-regexp-string (str)
  "Build pinyin regexp from STR."
  (my-ensure 'pinyinlib)
  (let* (rlt (i 0) ch)
    (while (< i (length str))
      (setq ch (elt str i))
      (setq rlt (concat rlt
                        (cond
                         ((and (<= ?a ch) (<= ch ?z))
                          (pinyinlib-build-regexp-char ch))
                         (t
                          (char-to-string ch)))))
      (setq i (1+ i)))
    rlt))

(defvar my-disable-idle-timer nil
  "Function passed to `my-run-with-idle-timer' is run immediately.")

(defun my-run-with-idle-timer (seconds func)
  "After SECONDS, run function FUNC once."
  (cond
   (my-disable-idle-timer
    (funcall func))
   (t
    (run-with-idle-timer seconds nil func))))

(defun my-imenu-item-position (item)
  "Handle some strange imenu ITEM."
  (if (markerp item) (marker-position item) item))

(defun my-closest-imenu-item-internal (cands)
  "Return closest imenu item from CANDS."
  (let* ((pos (point))
         closest)
    (dolist (c cands)
      (let* ((item (cdr c))
             (m (cdr item)))
        (when (and m (<= (my-imenu-item-position m) pos))
          (cond
           ((not closest)
            (setq closest item))
           ((< (- pos (my-imenu-item-position m))
               (- pos (my-imenu-item-position (cdr closest))))
            (setq closest item))))))
    closest))

(defun my-mark-to-position (&optional position)
  "Mark text from point to POSITION or end of of line."
  (set-mark (or position (line-end-position)))
  (activate-mark))

(defun my-closest-imenu-item ()
  "Return the closest imenu item."
  (my-ensure 'counsel)
  (my-closest-imenu-item-internal (counsel--imenu-candidates)))

(defun my-setup-extra-keymap (extra-fn-list hint fn &rest args)
  "Map EXTRA-FN-LIST to new keymap and show HINT after calling FN with ARGS."
  (let ((echo-keystrokes nil))
    (apply fn args)
    (message hint)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (dolist (item extra-fn-list)
         (define-key map (kbd (nth 0 item)) (nth 1 item)))
       map)
     t)))

;; @see http://emacs.stackexchange.com/questions/14129/which-keyboard-shortcut-to-use-for-navigating-out-of-a-string
(defun my-font-face-similar-p (f1 f2)
  "Font face F1 and F2 are similar or same."
  ;; (message "f1=%s f2=%s" f1 f2)
  ;; in emacs-lisp-mode, the '^' from "^abde" has list of faces:
  ;;   (font-lock-negation-char-face font-lock-string-face)
  (if (listp f1) (setq f1 (nth 1 f1)))
  (if (listp f2) (setq f2 (nth 1 f2)))

  (or (eq f1 f2)
      ;; C++ comment has different font face for limit and content
      ;; f1 or f2 could be a function object because of rainbow mode
      (and (string-match "-comment-" (format "%s" f1))
           (string-match "-comment-" (format "%s" f2)))))

(defun my-font-face-at-point-similar-p (font-face-list)
  "Test if font face at point is similar to any font in FONT-FACE-LIST."
  (let* ((f (get-text-property (point) 'face))
         rlt)
    (dolist (ff font-face-list)
      (if (my-font-face-similar-p f ff) (setq rlt t)))
    rlt))


(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
