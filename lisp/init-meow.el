;;; -*- lexical-binding: t -*-
;;; init-meow.el --- Settings for meow  -*- lexical-binding: t -*-

;; Filename: init-meow.el
;; Description: Settings for meow
;; Author: KiteAB <kiteabpl@outlook.com> (https://kiteab.ga)
;; Maintainer: KiteAB <kiteabpl@outlook.com> (https://kiteab.ga)
;; Copyright (C) 2021, KiteAB, all rights reserved.
;; Created: 2021-01-06 17:17:02
;; Last-Updated: 2021-02-17 22:02:25
;;           By: KiteAB
;; URL: https://github.com/KiteAB/.emacs.d/blob/master/site-lisp/config/init-meow.el
;; Keywords:
;;
;; Features that might be required by this library:
;;
;;
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;; Require




;; ;; Code:
;; (meow-global-mode 1)

;; ;; Motions
;; (defun kiteab/meow-forward-char ()
;;   (interactive)
;;   (when (< (point) (line-end-position))
;;     (forward-char (abs (prefix-numeric-value nil)))))

;; (defun kiteab/meow-backward-char ()
;;   (interactive)
;;   (when (> (point) (line-beginning-position))
;;     (backward-char (abs (prefix-numeric-value nil)))))

;; (defun kiteab/meow-forward-char-and-insert ()
;;   (interactive)
;;   (kiteab/meow-forward-char)
;;   (meow-insert))

;; (defun kiteab/meow-insert-at-end ()
;;   (interactive)
;;   (end-of-line)
;;   (meow-insert))

;; (defun kiteab/meow-down-5-lines ()
;;   (interactive)
;;   (next-line 5))

;; (defun kiteab/meow-insert-at-beginning ()
;;   (interactive)
;;   (beginning-of-line)
;;   (meow-insert))

;; (defun kiteab/meow-backward-char-and-normal ()
;;   (interactive)
;;   (kiteab/meow-backward-char)
;;   (meow-insert-exit))

;; (defun kiteab/meow-newline-and-insert ()
;;   (interactive)
;;   (end-of-line)
;;   (newline-and-indent)
;;   (meow-insert))

;; (defun kiteab/meow-special-newline ()
;;   (interactive)
;;   (newline-and-indent)
;;   (meow-insert))

;; (defun kiteab/meow-newline-up-and-insert ()
;;   (interactive)
;;   (beginning-of-line)
;;   (newline-and-indent)
;;   (previous-line)
;;   (indent-for-tab-command)
;;   (meow-insert))

;; (defun kiteab/meow-forward-word ()
;;   (interactive)
;;   (forward-word)
;;   (forward-char))

;; (defun kiteab/meow-forward-5-words ()
;;   (interactive)
;;   (forward-word 5)
;;   (forward-char))

;; (defun kiteab/meow-up-5-lines ()
;;   (interactive)
;;   (previous-line 5))

;; (defun kiteab/meow-last-buffer ()
;;   (interactive)
;;   (if (region-active-p)
;;       (deactivate-mark t)
;;     (meow-last-buffer nil)))

;; ;; Main Function
;; (defun meow-setup ()
;;   ;; Basic Layout Set
;;   (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)

;;   ;; Insert Mode
;;   (define-key meow-insert-state-keymap (kbd "<escape>") #'kiteab/meow-backward-char-and-normal)

;;   ;; Motion Mode
;;   (meow-motion-overwrite-define-key
;;    ;; Basic Movement
;;    '("e" . next-line)
;;    '("u" . previous-line))

;;   ;; Normal Mode
;;   (meow-normal-define-key
;;    ;; Number Arguments
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("0" . meow-digit-argument)

;;    '(":" . counsel-M-x)
;;    '(";" . counsel-M-x)
;;    '("/" . swiper)
;;    '("?" . help-command)
;;    '("RET" . mark-defun)
;;    '("DEL" . backward-char)
;;    '("<escape>" . kiteab/meow-last-buffer)
;;    '("a" . kiteab/meow-forward-char-and-insert)
;;    '("A" . kiteab/meow-insert-at-end)
;;    '("b" . backward-word)
;;    '("c" . meow-change)
;;    '("C" . meow-change-save)
;;    '("d" . meow-kill)
;;    '("e" . next-line)
;;    '("E" . kiteab/meow-down-5-lines)
;;    '("C-e" . scroll-up-command)
;;    '("f" . eval-defun)
;;    '("g" . meow-keyboard-quit)
;;    '("G" . goto-line)
;;    '("h" . help-command)
;;    ;'("i" . kiteab/meow-forward-char)
;;    '("I" . end-of-line)
;;    '("j" . eval-buffer)
;;    '("i" . meow-insert)
;;    '("K" . kiteab/meow-insert-at-beginning)
;;    '("l" . undo-only)
;;    '("L" . undo-tree-visualize)
;;    '("m" . eval-region)
;;    '("n" . kiteab/meow-backward-char)
;;    '("N" . beginning-of-line)
;;    '("o" . kiteab/meow-newline-and-insert)
;;    '("O" . kiteab/meow-newline-up-and-insert)
;;    '("C-o" . kiteab/meow-special-newline)
;;    '("p" . yank)
;;    '("q" . kmacro-start-macro)
;;    '("Q" . kill-current-buffer)
;;    '("C-q" . kmacro-end-or-call-macro)
;;    '("r" . query-replace)
;;    '("s" . meow-search)
;;    '("S" . save-buffer)
;;    '("t" . eval-last-sexp)
;;    '("u" . previous-line)
;;    '("U" . kiteab/meow-up-5-lines)
;;    '("C-u" . scroll-down-command)
;;    '("v" . set-mark-command)
;;    '("V" . mark-line)
;;    '("w" . kiteab/meow-forward-word)
;;    '("W" . kiteab/meow-forward-5-words)
;;    '("x" . meow-delete)
;;    '("y" . kill-ring-save)
;;    '("z" . meow-cheatsheet))

;;   ;; Leader Key
;;   ;; Prefix Commands
;;   (define-prefix-command 'meow-g-command)
;;   (define-prefix-command 'meow-q-command)
;;   (define-prefix-command 'meow-t-command)
;;   (define-prefix-command 'meow-w-command)
;;   (define-prefix-command 'meow-u-command)
;;   (meow-leader-define-key
;;    ;; Basic
;;    '("f" . counsel-find-file)
;;    '("g" . meow-g-command)
;;    '("gg" . beginning-of-buffer)
;;    '("gf" . xref-find-definitions)
;;    '("G" . end-of-buffer)
;;    '("Q" . emacs-session-save)

;;    ;; Functions
;;    '("q"  . meow-q-command)
;;    '("qc" . open-config-folder)
;;    '("qC" . open-require-file)
;;    '("qa" . set-alpha)
;;    '("qs" . kiteab/open-scratch)
;;    '("qT" . kiteab/add-todo-in-code)
;;    '("qi" . kiteab/change-indent-type)
;;    '("qp" . kiteab/provide-feature-name)
;;    '("qt" . kiteab/insert-current-date-time)
;;    '("qu" . kiteab/upgrade-modules)
;;    '("q TAB" . kiteab/open-erc)
;;    '("q SPC" . kiteab/format-commit)

;;    ;; Buffer
;;    '("t"  . meow-t-command)
;;    '("ts" . counsel-switch-buffer)
;;    '("tk" . kill-buffer)

;;    ;; Window
;;    '("w"  . meow-w-command)
;;    '("w1" . delete-other-windows)
;;    '("w2" . split-window-below)
;;    '("w3" . split-window-right)
;;    '("w0" . delete-window)
;;    '("wo" . other-window)

;;    ;; Useful
;;    '("u"  . meow-u-command)
;;    '("uf" . mark-defun)
;;    '("ul" . undo)
;;    '("ur" . query-replace)
;;    '("ue" . eshell)
;;    '("up" . list-processes)
;;    '("ug" . goto-line)
;;    '("ua" . align-regexp)

;;    ;; Plugins
;;    ;; Magit
;;    '("m" . magit-status)

;;    ;; Evil Nerd Commenter
;;    '("c" . evilnc-comment-or-uncomment-lines)
;;    ))

;; (setq meow-cursor-type-insert '(bar . 2))

;; (setq meow-esc-delay 0.001)

;; (meow-setup)

;; (with-eval-after-load 'meow-core
;;   (require 'meow)
;;   (meow-global-mode)
;;   (meow-esc-mode)
;;   (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;   (meow-setup-line-number)
;;   (meow-leader-define-key
;;    ;; Use SPC (0-9) for digit arguments.
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("0" . meow-digit-argument))
;;   (meow-normal-define-key
;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)
;;    '("-" . negative-argument)
;;   '("/" . meow-reverse)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("[" . meow-beginning-of-thing)
;;    '("]" . meow-end-of-thing)
;;    '("a" . meow-append)
;;    '("A" . meow-open-below)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("C" . meow-change-save)
;;    '("d" . meow-delete)
;;    '(";" . meow-line)
;;    '("f" . meow-find)
;;    '("F" . meow-find-expand)
;;    '("g" . meow-keyboard-quit)
;;    '("G" . meow-goto-line)
;;    '("h" . meow-left)
;;    '("H" . meow-left-expand)
;;    '("i" . meow-insert)
;;    '("I" . meow-open-above)
;;    '("m" . meow-join)
;;    '("M" . delete-indentation)
;;    '("x" . meow-kill)
;;    '("t" . meow-till)
;;    '("T" . meow-till-expand)
;;    '("s" . meow-mark-word)
;;    '("S" . meow-mark-symbol)
;;    '("j" . meow-next)
;;    '("J" . meow-next-expand)
;;    '("o" . meow-block)
;;    '("O" . meow-block-expand)
;;    '("k" . meow-prev)
;;    '("K" . meow-prev-expand)
;;    '("q" . meow-quit)
;;    '("r" . meow-replace)
;;    '("R" . meow-replace-save)
;;    '("n" . meow-search)
;;    '("N" . meow-pop-search)
;;    '("l" . meow-right)
;;    '("L" . meow-right-expand)
;;    '("u" . undo)
;;    '("v" . meow-visit)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("y" . meow-save)
;;    '("p" . meow-yank)
;;    '("z" . meow-pop-selection)
;;    '("Z" . meow-pop-all-selection)
;;    '("w" . meow-query-replace)
;;    '("W" . meow-query-replace-regexp)
;;    '("<escape>" . meow-last-buffer))
;;   )

(defun  meow-setup ()
  (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-ansi)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  ;;希望用leader键 spc + 对应的leader 键来执行对应命令
  (meow-leader-define-key
   '("'" . meow-wrap-string)
   '("(" . meow-wrap-round)
   '("[" . meow-wrap-square)
   '("{" . meow-wrap-curly)
   '("}" . meow-forward-barf)
   '(")" . meow-forward-slurp)
   '("e" . meow-eval-last-exp)
   '("E" . eldoc-mode)
   '("r" . meow-raise-sexp)
   '("S" . meow-split-sexp)
   '("s" . meow-splice-sexp)
   '("t" . meow-transpose-sexp)
   '("j" . meow-join-sexp)
   '("," . meow-pop-marker)
   '("." . meow-find-ref)
   '(";" . meow-comment)
   '("@ u" . smerge-keep-upper)
   '("@ l" . smerge-keep-lower)
   '("@ a" . smerge-keep-all)
   '("@ m" . smerge-keep-mine)
   '("@ o" . smerge-keep-other)
   '("@ @" . smerge-next)
   '("d" . dired)
   '("o" . delete-other-windows)
   '("L" . display-line-numbers-mode)
   '("k" . kill-buffer)
   '("w" . ace-window)
   '("W" . +rotate-window)
   '("o" . delete-other-windows)
   '("q" . delete-window)
   '("v" . magit)
   '("$" . +change-theme)
   '("-" . split-window-below)
   '("\\" . split-window-right)
   '("p" . project-find-file)
   '("b" . switch-to-buffer)
   '("a" . deadgrep)
   '("f" . find-file)
   '("i" . imenu))

  ;;希望前缀来执行的命令
  (define-prefix-command 'meow-g-command)
  (define-prefix-command 'meow-f-command)
  (meow-normal-define-key
   '("*" . meow-expand-0)
   '("=" . meow-expand-9)
   '("!" . meow-expand-8)
   '("[" . meow-expand-7)
   '("]" . meow-expand-6)
   '("{" . meow-expand-5)
   '("+" . meow-expand-4)
   '("}" . meow-expand-3)
   '(")" . meow-expand-2)
   '("(" . meow-expand-1)
   '("1" . digit-argument)
   '("2" . digit-argument)
   '("3" . digit-argument)
   '("4" . digit-argument)
   '("5" . digit-argument)
   '("6" . digit-argument)
   '("7" . digit-argument)
   '("8" . digit-argument)
   '("9" . digit-argument)
                                        ;'("0" . digit-argument)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   ;; '("a" . meow-append)
   '("a" . avy-goto-line)
   '("A" . meow-open-below)
                                        ;'("b" . meow-back-word)
   '("B" . meow-back-symbol)
                                        ;'("c" . meow-change)
   '("C" . meow-change-save)
   ;'("d" . meow-C-d)
   '("D" . meow-backspace)
   '("e" . meow-line)
   '("E" . meow-kmacro-lines)
   '("f" . meow-f-command)
   '("ff" . meow-find)
   '("fr" . counsel-recentf)
   '("fc" . meow-cancel)
   '("fd" . dired)
   '("fs" . counsel-switch-buffer)
   '("ft" . my-random-color-theme)
   '("fw" . plain-org-wiki)
   '("F" . meow-find-expand)
   ;; '("G" . meow-grab)
   '("g" . meow-g-command)
   ;; '("ga" . avy-goto-line)
   '("gc" . ace-pinyin-jump-char-2)
   '("gg" . beginning-of-buffer)
   '("gf" . xref-find-definitions)
   '("G" . end-of-buffer)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   ;; '("j" . meow-join)
   '("j" . evil-join)
   '("J" . meow-extend)
   '("k" . meow-kill)
   '("K" . meow-kill-append)
   '("l" . meow-till)
   '("L" . meow-till-expand)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-block-expand)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . emacs-session-save)
   ;'("G" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("/" . swiper)
   '("d" . meow-delete)
   ;; '(";" . counsel-M-x)
      '(";" . smex)
   '("0" . beginning-of-line)
   '("$" . end-of-line)
   '("c" . evilnc-comment-or-uncomment-lines)
                                        ;'("s" . meow-search)
   '("s" . aweshell-new)
   ;; '("S" . meow-pop-search)
   '("S" . meow-save)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
                                        ;'("V" . meow-kmacro-matches)
   '("V" . mark-line)
   ;; '("w" . meow-next-word)
   '("w" . ace-window)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("Y" . meow-yank-pop)
   '("z" . meow-pop)
   '("Z" . meow-pop-all-selection)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("&" . meow-query-replace)
   ;; '("%" . meow-query-replace-regexp)
   '("@" . recenter-top-bottom)
   '("^" . meow-pop-to-mark)
   '("'" . repeat)
   '("<escape>" . meow-last-buffer)
   '("\\" . quoted-insert)
   '("<f3>" . meow-start-kmacro)
   '("<f4>" . meow-end-or-call-kmacro)))

(setq
 meow-esc-delay 0.001
 meow-keypad-describe-delay 0.5
 meow-select-on-change t
 meow-selection-command-fallback '((meow-replace . meow-replace-char)
                                   (meow-change . meow-change-char)
                                   (meow-save . meow-save-empty)
                                   (meow-kill . meow-C-k)
                                   (meow-cancel . meow-keyboard-quit)
                                   (meow-cancel-selection . meow-keyboard-quit)
                                   (meow-pop . meow-pop-grab)
                                   (meow-reverse . meow-page-down)
                                   (meow-delete . meow-C-d)
                                   (meow-kmacro . meow-start-kmacro)))

(require 'meow)

(setq-default meow-normal-mode 1)
(with-eval-after-load "meow"
  (add-hook 'meow-mode-hook 'meow-esc-mode)
  (add-to-list 'meow-grab-fill-commands 'eval-expression)
  (add-to-list 'meow-char-thing-table '(?> . line))
  (add-to-list 'meow-char-thing-table '(?< . line))
  (meow-setup)
  (meow-setup-indicator))

;;meow表情
(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :config
  (meow-setup)
  (meow-setup-line-number)
  (meow-setup-indicator)
  (setq meow-replace-state-name-list
        '((normal . "Ꮚ•-•Ꮚ")
 		  (insert . "Ꮚ`-´Ꮚ")
 		  (keypad . "Ꮚ'-'Ꮚ")
 		  (motion . "Ꮚ-^-Ꮚ"))))

(provide 'init-meow)
;;; init-meow.el ends here
