;;; packages.el --- lispyville layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: 鲁亚军 <guanghui8827@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lispyville-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lispyville/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lispyville/pre-init-PACKAGE' and/or
;;   `lispyville/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst lispyville-packages
  '( (use-package lispyville
       :defer t
       :init
       (spacemeow//add-hooks-for-lispyville)
       :config
       (spacemeow//set-lispyville-keytheme)
       ))
  "The list of Lisp packages required by the lispyville layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun spacemeow//add-hooks-for-lispyville ()
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispyville-mode 1)))
  (add-hook 'ielm-mode-hook (lambda () (lispyville-mode 1)))
  (add-hook 'inferior-emacs-lisp-mode-hook (lambda () (lispyville-mode 1)))
  ;; (add-hook 'spacemacs-mode-hook (lambda () (lispyville-mode 1)))
  (add-hook 'clojure-mode-hook (lambda () (lispyville-mode 1)))
  (add-hook 'scheme-mode-hook (lambda () (lispyville-mode 1)))
  (add-hook 'cider-repl-mode-hook (lambda () (lispyville-mode 1))))

(defun spacemeow//set-lispyville-keytheme ()
  (with-eval-after-load 'lispyville
    (lispyville-set-key-theme '(operators
                                additional-movement
                                slurp/barf-lispy
                                additional
                                mark))
    (spacemacs/set-leader-keys "o." 'lispyville-mode)))

;;; packages.el ends here
