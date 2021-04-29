;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

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
;; Org configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)
(require 'init-hydra)

(defun zilongshanren/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(add-hook 'org-mode-hook #'zilongshanren/org-ispell)
(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(if (spacemacs/system-is-mswindows)
    (setq
     org-agenda-dir "f:/org-notes"
     deft-dir "f:/org-notes"
     blog-admin-dir "f:/zilongshanren.com")
  (setq
   org-agenda-dir "~/Documents/坚果云/我的坚果云/github/org-files"
   deft-dir "~/Documents/坚果云/我的坚果云/github/org-files"
   blog-admin-dir "~/zilongshanren.com"))

;; define the refile targets
(setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-gtd (expand-file-name "todo.org" org-agenda-dir))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
(setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
(setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
(setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
(setq org-agenda-files (list org-agenda-dir))

;; org-roam
(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :preface
  (defun hot-expand (str &optional mod)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
           :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("j" (hot-expand "<s" "java") "java")
     ("b" (hot-expand "<s" "scala") "scala")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "R :results output :exports both") "R")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-mode . (lambda ()
                       "Beautify org symbols."
                       (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)
                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :config
  ;; To speed up startup, don't put to init section
  (setq org-agenda-files `(,centaur-org-directory)
        org-todo-keywords
        '(
          (sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
          ;; (sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
                                        ;(sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")
          (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)")
          )
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t)

  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (xwidget-webkit-browse-url (concat "file://" file))
              (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
                (when (buffer-live-p buf)
                  (and (eq buf (current-buffer)) (quit-window))
                  (pop-to-buffer buf)))))
          org-file-apps))

  ;; Add gfm/md backends
  ;; (use-package ox-gfm)
  ;; (add-to-list 'org-export-backends 'md)

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; Prettify UI
  ;; (use-package org-bullets
  ;;   :if (char-displayable-p ?⚫)
  ;;   :hook (org-mode . org-bullets-mode)
  ;;   :init (setq org-bullets-bullet-list '("⚫" "⚫" "⚫" "⚫")))

  ;; (use-package org-fancy-priorities
  ;;   :diminish
  ;;   :hook (org-mode . org-fancy-priorities-mode)
  ;;   :init (setq org-fancy-priorities-list
  ;;               (if (char-displayable-p ?⯀)
  ;;                   '("⯀" "⯀" "⯀" "⯀")
  ;;                 '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (R . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  ;; (use-package ob-go
  ;; :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  ;; Use mermadi-cli: npm install -g @mermaid-js/mermaid-cli
  ;; (use-package ob-mermaid
  ;;   :init (cl-pushnew '(mermaid . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  ;; (use-package org-rich-yank
  ;;   :bind (:map org-mode-map
  ;;          ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  ;; (use-package org-mime
  ;;   :bind (:map message-mode-map
  ;;          ("C-c M-o" . org-mime-htmlize)
  ;;          :map org-mode-map
  ;;          ("C-c M-o" . org-mime-org-buffer-htmlize)))

  ;; Preview
  (use-package org-preview-html
    :diminish)

  ;; Presentation
  ;; (use-package org-tree-slide
  ;;   :diminish
  ;;   :functions (org-display-inline-images
  ;;               org-remove-inline-images)
  ;;   :bind (:map org-mode-map
  ;;          ("s-<f7>" . org-tree-slide-mode)
  ;;          :map org-tree-slide-mode-map
  ;;          ("<left>" . org-tree-slide-move-previous-tree)
  ;;          ("<right>" . org-tree-slide-move-next-tree)
  ;;          ("S-SPC" . org-tree-slide-move-previous-tree)
  ;;          ("SPC" . org-tree-slide-move-next-tree))
  ;;   :hook ((org-tree-slide-play . (lambda ()
  ;;                                   (text-scale-increase 4)
  ;;                                   (org-display-inline-images)
  ;;                                   (read-only-mode 1)))
  ;;          (org-tree-slide-stop . (lambda ()
  ;;                                   (text-scale-increase 0)
  ;;                                   (org-remove-inline-images)
  ;;                                   (read-only-mode -1))))
  ;;   :config
  ;;   (org-tree-slide-simple-profile)
  ;;   (setq org-tree-slide-skip-outline-level 2))

  ;; Pomodoro
  ;; (use-package org-pomodoro
  ;;   :custom-face
  ;;   (org-pomodoro-mode-line ((t (:inherit warning))))
  ;;   (org-pomodoro-mode-line-overtime ((t (:inherit error))))
  ;;   (org-pomodoro-mode-line-break ((t (:inherit success))))
  ;;   :bind (:map org-agenda-mode-map
  ;;               ("P" . org-pomodoro)))
  )
;; (when (and emacs/>=26p (executable-find "cc"))
;;   (use-package org-roam
;;     :diminish
;;     :custom (org-roam-directory centaur-org-directory)
;;     :hook (after-init . org-roam-mode)
;;     :bind (:map org-roam-mode-map
;;                 (("C-c n l" . org-roam)
;;                  ("C-c n f" . org-roam-find-file)
;;                  ("C-c n g" . org-roam-graph))
;;                 :map org-mode-map
;;                 (("C-c n i" . org-roam-insert))
;;                 (("C-c n I" . org-roam-insert-immediate)))
;;     )

;;   (use-package org-roam-server
;;     :functions xwidget-buffer xwidget-webkit-current-session
;;     :hook (org-roam-server-mode . org-roam-server-browse)
;;     :init
;;     (defun org-roam-server-browse ()
;;       (when org-roam-server-mode
;;         (let ((url (format "http://%s:%d" org-roam-server-host org-roam-server-port)))
;;           (if (featurep 'xwidget-internal)
;;               (progn
;;                 (xwidget-webkit-browse-url url)
;;                 (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
;;                   (when (buffer-live-p buf)
;;                     (and (eq buf (current-buffer)) (quit-window))
;;                     (pop-to-buffer buf))))
;;             (browse-url url)))))))

;; (defun run-python-first (&rest args)
;;   "Start a inferior python if there isn't one."
;;   (or (comint-check-proc "*Python*") (run-python)))

;; (advice-add 'org-babel-execute:ipython :after
;;             (lambda (body params)
;;               "Send body to `inferior-python'."
;;               (run-python-first)
;;               (python-shell-send-string body)))
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (setq-local completion-at-point-functions
;;                         '(pcomplete-completions-at-point python-completion-at-point))))
;; ;;这个函数很重要！
;; (add-hook 'ipython-mode-hook
;;           (lambda ()
;;             (setq-local completion-at-point-functions
;;                         '(pcomplete-completions-at-point python-completion-at-point))))
;; ;; (add-hook 'python-mode-hook
;; ;;           (lambda ()
;; ;;             (setq-local completion-at-point-functions
;; ;;                         '(pcomplete-completions-at-point python-completion-at-point))))

;(define-key evil-insert-state-map (kbd "C-c C-d") 'completion-at-point)

;; (defun ob-ipython-eldoc-function ()
;;   (when (org-babel-where-is-src-block-head)
;;     (python-eldoc-function)))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (setq-default eldoc-documentation-function 'ob-ipython-eldoc-function)))

(with-eval-after-load 'org
  (setq org-odt-preferred-output-format "docx") ;ODT转换格式默认为docx
  (setq org-startup-folded nil)                 ;默认展开内容
  (setq org-startup-indented t)                 ;默认缩进内容

  (defun org-export-docx ()
    (interactive)
    (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
          (template-file (concat (file-name-as-directory lazycat-emacs-root-dir)
                                 (file-name-as-directory "template")
                                 "template.docx")))
      (shell-command (format "pandoc %s -o %s --reference-doc=%s"
                             (buffer-file-name)
                             docx-file
                             template-file
                             ))
      (message "Convert finish: %s" docx-file))))

;; org-capture
(setq org-capture-templates
            '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
               "* TODO [#B] %?\n  %i\n %U"
               :empty-lines 1)
              ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
               "* %?\n  %i\n %U"
               :empty-lines 1)
              ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
               "* TODO [#B] %?\n  %i\n %U"
               :empty-lines 1)
              ("s" "Code Snippet" entry
               (file org-agenda-file-code-snippet)
               "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
              ("w" "work" entry (file+headline org-agenda-file-gtd "Work")
               "* TODO [#A] %?\n  %i\n %U"
               :empty-lines 1)
              ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
               :empty-lines 1)
              ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("j" "Journal Entry"
               entry (file+datetree org-agenda-file-journal)
               "* %?"
               :empty-lines 1)))

      (with-eval-after-load 'org-capture
        (defun org-hugo-new-subtree-post-capture-template ()
          "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
          (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
                 (fname (org-hugo-slug title)))
            (mapconcat #'identity
                       `(
                         ,(concat "* TODO " title)
                         ":PROPERTIES:"
                         ,(concat ":EXPORT_FILE_NAME: " fname)
                         ":END:"
                         "\n\n")        ;Place the cursor here finally
                       "\n")))

        (add-to-list 'org-capture-templates
                     '("h"              ;`org-capture' binding + h
                       "Hugo post"
                       entry
                       ;; It is assumed that below file is present in `org-directory'
                       ;; and that it has a "Blog Ideas" heading. It can even be a
                       ;; symlink pointing to the actual location of all-posts.org!
                       (file+headline org-agenda-file-blogposts "Blog Ideas")
                       (function org-hugo-new-subtree-post-capture-template))))

      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急
      (setq org-agenda-custom-commands
            '(
              ("w" . "任务安排")
              ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
              ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
              ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
              ("b" "Blog" tags-todo "BLOG")
              ("p" . "项目安排")
              ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"work\"")
              ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
              ("W" "Weekly Review"
               ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                ))))
;;; lazy-load
(dolist (hook (list
               'org-mode-hook
               ))
  (add-hook hook '(lambda ()
                    (require 'valign)
                    (valign-mode)

                    (setq truncate-lines nil) ;默认换行

                    (lazy-load-set-keys
                     '(
                       ("M-h" . set-mark-command) ;选中激活
                       )
                     org-mode-map
                     )
                    )))

(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-hide-emphasis-markers t)
;; (add-to-list 'org-emphasis-alist
;;              '("*" (:foreground "red")
;;                ))

(defface org-bold
  '((t :foreground "#d2268b"
       :background "#2e2e2e"
       :weight bold
       :underline t
       ))
  "Face for org-mode bold."
  :group 'org-faces)

(setq org-emphasis-alist
      '(("*" ;; (bold :foreground "Orange" )
         org-bold)
        ("/" italic)
        ("_" underline)
        ("=" ;; (:background "maroon" :foreground "white")
         org-verbatim verbatim)
        ("~" ;; (:background "deep sky blue" :foreground "MidnightBlue")
         org-code verbatim)
        ("+" (:strike-through t))))

;; Because spacemacs had different ideas about the verbatim background
(set-face-background 'org-bold "#2e2e2e")
(set-face-background 'org-verbatim "#2e2e2e")
(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))


(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
