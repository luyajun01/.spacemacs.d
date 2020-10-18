;; init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2009-2020 Vincent Zhang

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
;; Flycheck configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-funcs)

(use-package flycheck
  :diminish
  :commands flycheck-redefine-standard-error-levels
  :hook (after-init . global-flycheck-mode)
  :init (setq flycheck-global-modes
              '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                    org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
              flycheck-emacs-lisp-load-path 'inherit
              flycheck-indication-mode (if (display-graphic-p)
                                           'right-fringe
                                         'right-margin)
              ;; Only check while saving and opening files
              flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  ;; Prettify indication styles
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow)

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if emacs/>=26p
          (use-package flycheck-posframe
            :hook (flycheck-mode . flycheck-posframe-mode)
            :init (setq flycheck-posframe-inhibit-functions
                        '((lambda (&rest _) (bound-and-true-p company-backend)))))
        (use-package flycheck-pos-tip
          :defines flycheck-pos-tip-timeout
          :hook (global-flycheck-mode . flycheck-pos-tip-mode)
          :config (setq flycheck-pos-tip-timeout 30)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))


;; I don't like `global-flycheck-mode', some mode, such as elisp mode don't need.
(dolist (hook (list
               'org-mode-hook
               ;; 'python-mode-hook
               ;; 'swift-mode-hook
               ;; 'go-mode-hook
               ;; 'js-mode-hook
               ))
  (add-hook
   hook
   '(lambda ()
      ;; OS Config
      (when (featurep 'cocoa)
        ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
        (require 'exec-path-from-shell)
        (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GEM_PATH"))
        (exec-path-from-shell-initialize))

      (require 'flycheck)

      (setq-default flycheck-disabled-checkers ;disable json-jsonlist checking for json files
                    (append flycheck-disabled-checkers
                            '(json-jsonlist)))

      (setq-default flycheck-disabled-checkers ;disable jshint since we prefer eslint checking
                    (append flycheck-disabled-checkers
                            '(javascript-jshint)))

      (flycheck-add-mode 'javascript-eslint 'web-mode) ;use eslint with web-mode for jsx files

      (setq-default flycheck-temp-prefix ".flycheck")

      (with-eval-after-load 'flycheck
        (require 'flycheck-posframe)
        (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
      (flycheck-mode 1))))


(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
