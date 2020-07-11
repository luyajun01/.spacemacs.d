;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

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
;; Auto-completion configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :preface
  (defvar company-enable-yas centaur-company-enable-yas
    "Enable yasnippet for all backends.")

  (defun company-backend-with-yas (backend)
    (if (or (not company-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  ;; :bind (("M-/" . company-complete)
  ;;        ("C-c C-y" . company-yasnippet)
  ;;        :map company-active-map
  ;;        ("C-p" . company-select-previous)
  ;;        ("C-n" . company-select-next)
  ;;        ("TAB" . company-complete-common-or-cycle)
  ;;        ("<tab>" . company-complete-common-or-cycle)
  ;;        ("S-TAB" . company-select-previous)
  ;;        ("<backtab>" . company-select-previous)
  ;;        :map company-search-map
  ;;        ("C-p" . company-select-previous)
  ;;        ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (progn
    (global-company-mode t)
    (setq company-backends
          '((company-dabbrev
             company-abbrev
             company-yasnippet
company-anaconda
             )
            (company-capf)
          )
    ))
  ;; (setq company-tooltip-align-annotations t ; aligns annotation to the right
  ;;       company-tooltip-limit 10            ; bigger popup window
  ;;       company-idle-delay .2               ; decrease delay before autocompletion popup shows
  ;;       company-echo-delay 0                ; remove annoying blinking
  ;;       company-show-numbers t
  ;;       company-minimum-prefix-length 2
  ;;       company-require-match nil
  ;;       company-dabbrev-ignore-case nil
  ;;       company-dabbrev-downcase nil)

  ;; Popup documentation for completion candidates
  ;; (when (display-graphic-p)
  ;;   (use-package company-quickhelp
  ;;     :bind (:map company-active-map
  ;;                 ("M-h" . company-quickhelp-manual-begin))
  ;;     :hook (global-company-mode . company-quickhelp-mode)
  ;;     :config (setq company-quickhelp-delay 0.8))
  ;;   )

  ;; Support yas in commpany
  ;; Note: Must be the last to involve all backends
  (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

;; -*- coding: utf-8; lexical-binding: t; -*-

(add-hook 'after-init-hook 'global-company-mode)

(when (fboundp 'evil-declare-change-repeat)
  (mapc #'evil-declare-change-repeat
        '(company-complete-common
          company-select-next
          company-select-previous
          company-complete-selection
          company-complete-number)))

(eval-after-load 'company
  '(progn
     ;; @see https://github.com/company-mode/company-mode/issues/348
     (company-statistics-mode)
(add-to-list 'company-backends '(company-anaconda :with company-capf))
     (add-to-list 'company-backends 'company-cmake)
     (add-to-list 'company-backends 'company-c-headers)
     ;; can't work with TRAMP
     (setq company-backends (delete 'company-ropemacs company-backends))

     ;; company-ctags is faster than company-etags
     (require 'company-ctags)
     (company-ctags-auto-setup)

     ;; (setq company-backends (delete 'company-capf company-backends))

     ;; I don't like the downcase word in company-dabbrev!
     (setq company-dabbrev-downcase nil
           ;; make previous/next selection in the popup cycles
           company-selection-wrap-around t
           ;; Some languages use camel case naming convention,
           ;; so company should be case sensitive.
           company-dabbrev-ignore-case nil
           ;; press M-number to choose candidate
           company-show-numbers t
           company-idle-delay 0.2
           company-clang-insert-arguments nil
           company-require-match nil
           company-etags-ignore-case t
           ;; @see https://github.com/company-mode/company-mode/issues/146
           company-tooltip-align-annotations t)

     ;; @see https://github.com/redguardtoo/emacs.d/commit/2ff305c1ddd7faff6dc9fa0869e39f1e9ed1182d
     (defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
       ;; you can use (ad-get-arg 0) and (ad-set-arg 0) to tweak the arguments
       (if (memq major-mode '(php-mode html-mode web-mode nxml-mode))
           (setq ad-return-value nil)
         ad-do-it))

     ;; press SPACE will accept the highlighted candidate and insert a space
     ;; `M-x describe-variable company-auto-complete-chars` for details
     ;; That's BAD idea.
     (setq company-auto-complete nil)

     ;; NOT to load company-mode for certain major modes.
     ;; Ironic that I suggested this feature but I totally forgot it
     ;; until two years later.
     ;; https://github.com/company-mode/company-mode/issues/29
     (setq company-global-modes
           '(not
             eshell-mode comint-mode erc-mode gud-mode rcirc-mode
             minibuffer-inactive-mode))))

;{{ setup company-ispell
(defun toggle-company-ispell ()
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))

(defun company-ispell-setup ()
  ;; @see https://github.com/company-mode/company-mode/issues/50
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell)
    ;; https://github.com/redguardtoo/emacs.d/issues/473
    (cond
     ((and (boundp 'ispell-alternate-dictionary)
           ispell-alternate-dictionary)
      (setq company-ispell-dictionary ispell-alternate-dictionary))
     (t
       (setq company-ispell-dictionary (file-truename "~/.emacs.d/misc/english-words.txt"))))))

;message-mode use company-bbdb.
;So we should NOT turn on company-ispell
(add-hook 'org-mode-hook 'company-ispell-setup)
;; }}

;; Wait 30  minutes to update cache from tags file
;; assume `company-dabbrev-code' or `company-dabbrev' in the same group provides candidates
;; @see https://github.com/company-mode/company-mode/pull/877 for tech details
;; The purpose is to avoid rebuilding candidate too frequently because rebuilding could take
;; too much time.
(defvar company-etags-update-interval 1800
  "The interval (seconds) to update candidate cache.
Function `tags-completion-table' sets up variable `tags-completion-table'
by parsing tags files.
The interval stops the function being called too frequently.")

(defvar company-etags-timer nil
  "Timer to avoid calling function `tags-completion-table' too frequently.")
(use-package company-posframe
  :if window-system
  :hook (company-mode . company-posframe-mode)
  (ess-r-mode . company-posframe-mode)
  (r-mode . company-posframe-mode)
  (markdown-mode . company-posframe-mode)
  (org-mode . company-posframe-mode)
(python-mode . company-posframe-mode)
  )
(company-posframe-mode 1)
;; (defface company-posframe-quickhelp
;;   '((t :inherit highlight))
;;   "Face for company-posframe-quickhelp doc.
;; Fix: need improve.")
(require 'desktop) ;this line is needed.
(push '(company-posframe-mode . nil)
      desktop-minor-mode-table)
;;;company
  ;; company-english
  ;;  helm-company choose from company completions with C-:
  ;; (with-eval-after-load 'company
  ;;   (global-company-mode)
  ;;   (define-key company-mode-map (kbd "C-:") 'helm-company)
  ;;   (define-key company-active-map (kbd "C-:") 'helm-company)
  ;;   (setq ess-use-company t)
  ;;   ;; (require 'company-tabnine)
  ;;   ;; (add-to-list 'company-backends #'company-tabnine)
  ;;                                       ;以后要用的时候打开
  ;;   ;; (setq company-idle-delay 0)
  ;;   (setq company-show-numbers t)
  ;;   (setq company-frontends
  ;;         '(company-tng-frontend
  ;;           company-pseudo-tooltip-frontend
  ;;           company-echo-metadata-frontend))
  ;;   ;; Add yasnippet support for all company backends
  ;;   ;; https://github.com/syl20bnr/spacemacs/pull/179
  ;;   (defvar company-mode/enable-yas t
  ;;     "Enable yasnippet for all backends.")
  ;;   (defun company-mode/backend-with-yas (backend)
  ;;     (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
  ;;         backend
  ;;       (append (if (consp backend) backend (list backend))
  ;;               '(:with company-yasnippet))))
  ;;   (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)
  ;;         )
  ;;   ;;company-english-
  ;;   ;; (add-to-list 'load-path (expand-file-name "~/dot-spacemacs/private/company-english-helper"))
  ;;   ;; (require 'company-english-helper)
  ;;   ;; (setq company-english-helper-fuzz-search-p t);模糊匹配
  ;;   ;; (add-hook 'after-init-hook 'company-statistics-mode)
  ;;   ;;        ;; workaround for company-transformers
  ;;   ;; (setq company-tabnine--disable-next-transform nil)
  ;;   ;; (defun my-company--transform-candidates (func &rest args)
  ;;   ;;   (if (not company-tabnine--disable-next-transform)
  ;;   ;;       (apply func args)
  ;;   ;;     (setq company-tabnine--disable-next-transform nil)
  ;;   ;;     (car args)))
  ;;   ;; (defun my-company-tabnine (func &rest args)
  ;;   ;;   (when (eq (car args) 'candidates)
  ;;   ;;     (setq company-tabnine--disable-next-transform t))
  ;;   ;;   (apply func args))

  ;;   ;; (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
  ;;   ;; (advice-add #'company-tabnine :around #'my-company-tabnine)
  ;;   ;; (defun company//sort-by-tabnine (candidates)
  ;;   ;;   (if (or (functionp company-backend)
  ;;   ;;           (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
  ;;   ;;       candidates
  ;;   ;;     (let ((candidates-table (make-hash-table :test #'equal))
  ;;   ;;           candidates-1
  ;;   ;;           candidates-2)
  ;;   ;;       (dolist (candidate candidates)
  ;;   ;;         (if (eq (get-text-property 0 'company-backend candidate)
  ;;   ;;                 'company-tabnine)
  ;;   ;;             (unless (gethash candidate candidates-table)
  ;;   ;;               (push candidate candidates-2))
  ;;   ;;           (push candidate candidates-1)
  ;;   ;;           (puthash candidate t candidates-table)))
  ;;   ;;       (setq candidates-1 (nreverse candidates-1))
  ;;   ;;       (setq candidates-2 (nreverse candidates-2))
  ;;   ;;       (nconc (seq-take candidates-1 2)
  ;;   ;;              (seq-take candidates-2 2)
  ;;   ;;              (seq-drop candidates-1 2)
  ;;   ;;              (seq-drop candidates-2 2)))))
  ;;   ;; (add-to-list 'company-transformers 'company//sort-by-tabnine t)
  ;;   )

;; 可以利用数字筛选candidate
;; (defun ora-company-number ()
;;   "Forward to `company-complete-number'.

;; Unless the number is potentially part of the candidate.
;; In that case, insert the number."
;;   (interactive)
;;   (let* ((k (this-command-keys))
;;          (re (concat "^" company-prefix k)))
;;     (if (cl-find-if (lambda (s) (string-match re s))
;;                     company-candidates)
;;         (self-insert-command 1)
;;       (company-complete-number (string-to-number k)))))
;; (let ((map company-active-map))
;;   (mapc
;;    (lambda (x)
;;      (define-key map (format "%d" x) 'ora-company-number))
;;    (number-sequence 0 9))
;;   (define-key map " " (lambda ()
;;                         (interactive)
;;                         (company-abort)
;;                         (self-insert-command 1)))
;;   (define-key map (kbd "<return>") nil))
(use-package company-anaconda
  :after (anaconda-mode company)
  :config (add-to-list 'company-backends 'company-anaconda))

(provide 'init-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
