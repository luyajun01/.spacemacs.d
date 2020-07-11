;; init-yasnippet.el --- Initialize yasnippet configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Vincent Zhang

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
;; Yasnippet configurations.
;;

;;; Code:

;; (use-package yasnippet
;;   :diminish yas-minor-mode
;;   :hook (after-init . yas-global-mode)
;;   :config (use-package yasnippet-snippets))
;; -*- coding: utf-8; lexical-binding: t; -*-

;; my private snippets, should be placed before enabling yasnippet
(setq my-yasnippets (expand-file-name "~/dot-spacemacs/snippets"))

(defun yasnippet-generic-setup-for-mode-hook ()
  (unless (is-buffer-file-temp) (yas-minor-mode 1)))

(add-hook 'prog-mode-hook 'yasnippet-generic-setup-for-mode-hook)
(add-hook 'text-mode-hook 'yasnippet-generic-setup-for-mode-hook)
;; {{ modes do NOT inherit from prog-mode
(add-hook 'cmake-mode-hook 'yasnippet-generic-setup-for-mode-hook)
(add-hook 'web-mode-hook 'yasnippet-generic-setup-for-mode-hook)
(add-hook 'scss-mode-hook 'yasnippet-generic-setup-for-mode-hook)
;; }}

(defun my-yas-reload-all ()
  "Compile and reload yasnippets.  Run the command after adding new snippets."
  (interactive)
  (yas-compile-directory (file-truename "~/.emacs.d/snippets"))
  (yas-reload-all)
  (yas-minor-mode 1))

(defun my-yas-field-to-statement(str sep)
  "If STR=='a.b.c' and SEP=' && ', 'a.b.c' => 'a && a.b && a.b.c'"
  (let ((a (split-string str "\\.")) rlt)
    (mapconcat 'identity
               (mapcar (lambda (elem)
                         (cond
                          (rlt
                           (setq rlt (concat rlt "." elem)))
                          (t
                           (setq rlt elem)))) a)
               sep)))

(defun my-yas-get-first-name-from-to-field ()
  (let ((rlt "AGENT_NAME") str)
    (save-excursion
      (goto-char (point-min))
      ;; first line in email could be some hidden line containing NO to field
      (setq str (my-buffer-str)))
    ;; (message "str=%s" str)
    (if (string-match "^To: \"?\\([a-zA-Z]+\\)" str)
        (setq rlt (capitalize (match-string 1 str))))
    ;; (message "rlt=%s" rlt)
    rlt))

(defun my-yas-camelcase-to-string-list (str)
  "Convert camelcase STR into string list."
  (let* ((old-case case-fold-search) rlt)
    (setq case-fold-search nil)
    (setq rlt (replace-regexp-in-string "\\([A-Z]+\\)" " \\1" str t))
    (setq rlt (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]+\\)" "\\1 \\2" rlt t))
    ;; restore case-fold-search
    (setq case-fold-search old-case)
    (split-string rlt " ")))

(defun my-yas-camelcase-to-downcase (str)
  (let* ((l (my-yas-camelcase-to-string-list str))
         (case-fold-search nil))
    (mapconcat 'identity (mapcar (lambda (elem)
                                   (if (string-match "^[A-Z]+$" elem)
                                       elem
                                     (downcase elem)))
                                 l)
               " ")))

(defun my-yas-escape-string (s)
  (let* ((rlt (replace-regexp-in-string "'" "\\\\'" s)))
    (setq rlt (replace-regexp-in-string "\"" "\\\\\"" rlt))
    rlt))

(defun my-read-n-from-kill-ring ()
  (let* ((cands (subseq kill-ring 0 (min (read-number "fetch N `kill-ring'?" 1)
                                         (length kill-ring)))))
    (mapc (lambda (txt)
            (set-text-properties 0 (length txt) nil txt)
            txt)
          cands)))

(defun my-yas-get-var-list-from-kill-ring ()
  "Variable name is among the `kill-ring'.  Multiple major modes supported."
  (let* ((top-kill-ring (my-read-n-from-kill-ring))
         rlt)
    (cond
     ((memq major-mode '(js-mode javascript-mode js2-mode js3-mode rjsx-mode web-mode))
      (setq rlt (mapconcat (lambda (i) (format "'%s=', %s" (my-yas-escape-string i) i)) top-kill-ring ", ")))
     ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
      (setq rlt (concat (mapconcat (lambda (i) (format "%s=%%s" i)) top-kill-ring ", ")
                        "\" "
                        (mapconcat (lambda (i) (format "%s" i)) top-kill-ring " "))))
     ((memq major-mode '(c-mode c++-mode))
      (setq rlt (concat (mapconcat (lambda (i) (format "%s=%%s" i)) top-kill-ring ", ")
                        "\\n\", "
                        (mapconcat (lambda (i) (format "%s" i)) top-kill-ring ", "))))
     (t (setq rlt "")))
    rlt))
(add-auto-mode 'snippet-mode "\\.yasnippet\\'")

(eval-after-load 'yasnippet
  '(progn
     ;; http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
     (setq-default mode-require-final-newline nil)
     ;; (message "yas-snippet-dirs=%s" (mapconcat 'identity yas-snippet-dirs ":"))

     ;; Use `yas-dropdown-prompt' if possible. It requires `dropdown-list'.
     (setq yas-prompt-functions '(yas-dropdown-prompt
                                  yas-ido-prompt
                                  yas-completing-prompt))

     ;; use `yas-completing-prompt' when ONLY when `M-x yas-insert-snippet'
     ;; thanks to capitaomorte for providing the trick.
     (defadvice yas-insert-snippet (around use-completing-prompt activate)
       "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
       (let* ((yas-prompt-functions '(yas-completing-prompt)))
         ad-do-it))

     (when (and  (file-exists-p my-yasnippets)
                 (not (member my-yasnippets yas-snippet-dirs)))
       (add-to-list 'yas-snippet-dirs my-yasnippets))

     (yas-reload-all)))

(provide 'init-yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-yasnippet.el ends here


