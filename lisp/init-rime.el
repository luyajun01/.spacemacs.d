;; init-rime.el --- Configuration for emacs-rime

;; Filename: init-rime.el
;; Description: Configuration for emacs-rime
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-03-22 14:52:23
;; Version: 0.1
;; Last-Updated: 2020-03-22 14:52:23
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-rime.el
;; Keywords:
;; Compatibility: GNU Emacs 26.3
;;
;; Features that might be required by this library:
;;
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

;;; Commentary:
;;
;; Configuration for emacs-rime
;;

;;; Installation:
;;
;; Put init-rime.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-rime)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-rime RET
;;

;;; Change log:
;;
;; 2020/03/22
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'rime)

(use-package fcitx
              :after evil
              :config
              (when (executable-find "fcitx-remote")
                (fcitx-evil-turn-on)))

(use-package ace-pinyin
              :after avy
              :init (setq ace-pinyin-use-avy t)
              :config (ace-pinyin-global-mode t))


(defun +rime-force-enable ()
  "[ENHANCED] Force into Chinese input state.
If current input method is not `rime', active it first. If it is
currently in the `evil' non-editable state, then switch to
`evil-insert-state'."
  (interactive)
  (let ((input-method "rime"))
    (unless (string= current-input-method input-method)
      (activate-input-method input-method))
    (when (rime-predicate-evil-mode-p)
      (if (= (1+ (point)) (line-end-position))
          (evil-append 1)
        (evil-insert 1)))
    (rime-force-enable)))

(use-package rime
  :bind
  ("C-S-p" . #'+rime-convert-string-at-point)
  (:map rime-active-mode-map
        ("C-S-j" . 'rime-inline-ascii)
        ("C-M-S-s-j" . 'rime-inline-ascii))
  (:map rime-mode-map
        ("C-M-S-s-j" . #'rime-force-enable)
        ("C-." . #'rime-send-keybinding)
        ("S-SPC" . #'rime-send-keybinding)
        ("C-`" . #'rime-send-keybinding)
        ("C-~" . #'rime-send-keybinding)
        ("C-S-`" . #'rime-send-keybinding))
  :custom
  (rime-title "R")
  (rime-librime-root (expand-file-name "librime/dist" "~/.emacs.d"))
  (rime-show-candidate 'posframe)
  (rime-posframe-style 'simple)
  (rime-inline-ascii-trigger 'shift-l)
  (default-input-method "rime")
  :hook
  ((after-init kill-emacs) . (lambda ()
                               (when (fboundp 'rime-lib-sync-user-data)
                                 (ignore-errors (rime-sync)))))
  :config
  (add-hook 'org-mode 'markdown-mode 'beancount-mode)
(defun +rime-convert-string-at-point ()
   "Convert the string at point to Chinese using the current input scheme.
First call `+rime-force-enable' to active the input method, and
then search back from the current cursor for available string (if
a string is selected, use it) as the input code, call the current
input scheme to convert to Chinese."
   (interactive)
   (+rime-force-enable)
   (let ((string (if mark-active
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (buffer-substring-no-properties
                    (point) (max (line-beginning-position) (- (point) 80)))))
         code
         length)
     (cond ((string-match "\\([a-z]+\\|[[:punct:]]\\)[[:blank:]]*$" string)
            (setq code (replace-regexp-in-string
                        "^[-']" ""
                        (match-string 0 string)))
            (setq length (length code))
            (setq code (replace-regexp-in-string " +" "" code))
            (if mark-active
                (delete-region (region-beginning) (region-end))
              (when (> length 0)
                (delete-char (- 0 length))))
            (when (> length 0)
              (setq unread-command-events
                    (append (listify-key-sequence code)
                            unread-command-events))))
           (t (message "`+rime-convert-string-at-point' did nothing."))))
   )

(unless (fboundp 'rime--posframe-display-content)
  (error "Function `rime--posframe-display-content' is not available."))
  )

;; 默认值
(setq rime-translate-keybindings
      '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))

;;; Code:
(setq rime-user-data-dir "/Users/luyajun/Library/Rime")
(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "WenQuanYi Micro Hei Mono-14"
            :internal-border-width 10))



(setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p
        rime-predicate-prog-in-code-p))


(lazy-load-set-keys
 '(
   ("M-o" . rime--backspace)
   ("M-m" . rime--return)
   ("M-h" . rime--escape))
 rime-active-mode-map)





(provide 'init-rime)


;;; init-rime.el ends here
