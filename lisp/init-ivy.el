;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

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
;; Ivy configurations.
;;
;;; Code:
;;;打开ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode))

(use-package counsel
  :diminish ivy-mode counsel-mode
  :defines (projectile-completion-system magit-completing-read-function)
  :bind (
         ;("C-s" . swiper)
         ("C-S-s" . swiper-all)
         ("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf)
         ("C-x j" . counsel-mark-ring)
         ("C-c L" . counsel-load-library)
         ("C-c P" . counsel-package)
         ("C-c f" . counsel-find-library)
         ("C-c g" . counsel-grep)
         ("C-c h" . counsel-command-history)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c l" . counsel-locate)
         ("C-c r" . counsel-rg)
         ("C-c z" . counsel-fzf)
         ("C-c c L" . counsel-load-library)
         ("C-c c P" . counsel-package)
         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-find-library)
         ("C-c c g" . counsel-grep)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-locate)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c t" . counsel-load-theme)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         ("C-c c z" . counsel-fzf)
         ;; Find counsel commands quickly
         ("<f6>" . (lambda ()
                     (interactive)
                     (counsel-M-x "^counsel ")))

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         ;; Search at point
         ;; "M-j": word-at-point
         ;; "M-n"/"C-w": symbol-at-point
         ;; Refer to https://www.emacswiki.org/emacs/SearchAtPoint#toc8
         ;; and https://github.com/abo-abo/swiper/wiki/FAQ
         ;; ("C-w" . (lambda ()
         ;;            (interactive)
         ;;            (insert (format "%s" (with-ivy-window (ivy-thing-at-point))))))

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)
         :map swiper-map
         ("M-%" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)      ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  (setq ivy-format-function 'ivy-format-function-arrow)
  ;; (setq ivy-initial-inputs-alist nil)

  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))

  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")

  ;; Use faster search tools: ripgrep or the silver search
 ; (let ((command
 ;        (cond
 ;         ((executable-find "rg")
 ;          "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
 ;         ((executable-find "ag")
 ;          "ag -i --noheading --nocolor --nofilename --numbers '%s' %s")
 ;         (t counsel-grep-base-command))))
 ;   (setq counsel-grep-base-command command))

 ; (when (executable-find "rg")
 ;   (setq counsel-git-cmd "rg --files")
 ;   (setq counsel-rg-base-command
 ;         "rg -i -M 120 --no-heading --line-number --color never %s ."))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Enhance fuzzy matching
  (use-package flx)

  ;; Enhance M-x
  (use-package smex)

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :bind (:map ivy-minibuffer-map
                ("M-o" . ivy-dispatching-done-hydra)))

  (use-package ivy-rich
    :ensure t
    :config
    (setq ivy-rich--original-display-transformers-list nil) ;; needs to be set otherwise (ivy-rich-set-display-transformer) does not get called
    (ivy-set-display-transformer 'ivy-switch-buffer
                                 'ivy-rich-switch-buffer-transformer)
    (setq-default ivy-rich-path-style 'abbrev
                  ivy-virtual-abbreviate 'full
                  ivy-rich-switch-buffer-align-virtual-buffer t))

  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :ensure t
    :init
    ;; xref initialization is different in Emacs 27 - there are two different
    ;; variables which can be set rather than just one
    (when (>= emacs-major-version 27)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
    ;; commands other than xref-find-definitions (e.g. project-find-regexp)
    ;; as well
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
  
  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :after flyspell
    :bind (:map flyspell-mode-map
                ("C-;" . flyspell-correct-previous-word-generic)))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook (after-init . counsel-projectile-mode))

  ;; Stylesheet-selector-aware swiper
  (use-package counsel-css
    :bind (:map counsel-mode-map
                ("C-c c c" . counsel-css))
    :hook (css-mode . counsel-css-imenu-setup))

  ;; Display world clock using Ivy
  (use-package counsel-world-clock
    :bind (:map counsel-mode-map
                ("C-c c k" . counsel-world-clock)))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :bind (:map counsel-mode-map
                ("C-c c v" . counsel-tramp)))

  ;; Improve `counsel-ag', also impact `counsel-rg', `counsel-pt'.
  ;; search the selection or current symbol by default
 ; (eval-and-compile
 ;   (declare-function ivy-thing-at-point "ivy")
 ;   (defun my-counsel-ag(-counsel-ag &optional initial-input initial-directory extra-ag-args ag-prompt)
 ;     (unless initial-input
 ;       (if (region-active-p)
 ;           (setq initial-input (buffer-substring-no-properties
 ;                                (region-beginning) (region-end)))
 ;         (setq initial-input (ivy-thing-at-point))))
 ;     (unless initial-directory
 ;       (setq initial-directory default-directory))
 ;     (message "input: %s" initial-input)
 ;     (funcall -counsel-ag initial-input initial-directory extra-ag-args ag-prompt))
 ;   (advice-add 'counsel-ag :around #'my-counsel-ag))

  ;; Support pinyin in Ivy
  ;; Input prefix ':' to match pinyin
  ;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
  ;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
  (use-package pinyinlib
    :functions ivy--regex-plus ivy--regex-ignore-order
    :commands pinyinlib-build-regexp-string
    :preface
    (defun re-builder-pinyin (str)
      (or (pinyin-to-utf8 str)
          (ivy--regex-plus str)
          (ivy--regex-ignore-order str)))
    (defun my-pinyinlib-build-regexp-string (str)
      (cond ((equal str ".*")
             ".*")
            (t
             (pinyinlib-build-regexp-string str t))))
    (defun my-pinyin-regexp-helper (str)
      (cond ((equal str " ")
             ".*")
            ((equal str "")
             nil)
            (t
             str)))
    (defun pinyin-to-utf8 (str)
      (cond ((equal 0 (length str))
             nil)
            ((equal (substring str 0 1) ":")
             (mapconcat 'my-pinyinlib-build-regexp-string
                        (remove nil (mapcar 'my-pinyin-regexp-helper
                                            (split-string
                                             (replace-regexp-in-string ":" "" str ) "")))
                        ""))
            (t
             nil)))
    :init (setq ivy-re-builders-alist
                '((read-file-name-internal . ivy--regex-fuzzy)
                  (t . re-builder-pinyin))))
  )

(defun avy-goto-word-0-in-line ()
  (interactive)
  (avy-goto-word-0 nil (point-at-bol) (point-at-eol)))

(defun avy-goto-word-1-in-line (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-goto-word-1 char arg (point-at-bol) (point-at-eol) nil))
(defun avy-goto-word-1-backward-in-line (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-goto-word-1 char arg (point-at-bol) (point) nil))

(defun avy-goto-word-1-forward-in-line (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-goto-word-1 char arg (point) (point-at-eol) nil))
;(global-set-key (kbd "C-c j") 'avy-goto-word-1-in-line)
;; ivy-posframe
 ;; display at `ivy-posframe-style'
 ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
 ;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
 ;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
 ;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
 ;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
 ;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
 ;; (ivy-posframe-mode 1)
 ;; Per-command mode
 (require 'ivy-posframe)
 ;; Different command can use different display function.
 (setq ivy-posframe-display-functions-alist
       '((swiper          . ivy-posframe-display-at-window-bottom-left)
         (complete-symbol . ivy-posframe-display-at-point)
         (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
         (t               . ivy-posframe-display)))
 (ivy-posframe-mode 1)
(provide 'init-ivy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
