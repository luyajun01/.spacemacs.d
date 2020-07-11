;; (defun zilongshanren-programming/init-lispy ()
;;   (use-package lispy
;;     :defer t
;;     :init
;;     (progn
;;       ;; (push '(cider-repl-mode . ("[`'~@]+" "#" "#\\?@?")) lispy-parens-preceding-syntax-alist
;;       ;;       )

;;       (spacemacs|hide-lighter lispy-mode)
;;       (define-key lispy-mode-map (kbd "M-s") 'lispy-splice)
;;       (define-key lispy-mode-map (kbd "s-k") 'paredit-splice-sexp-killing-backward)

;;       (with-eval-after-load 'cider-repl
;;         (define-key cider-repl-mode-map (kbd "C-s-j") 'cider-repl-newline-and-indent))

;;       (add-hook
;;        'minibuffer-setup-hook
;;        'conditionally-enable-lispy)
;;       (define-key lispy-mode-map (kbd "s-m") 'lispy-mark-symbol)
;;       (define-key lispy-mode-map (kbd "s-u") 'lispy-undo)
;;       (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
;;       (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))
;;     (progn
;;       (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;;       (add-hook 'ielm-mode-hook (lambda () (lispy-mode 1)))
;;       (add-hook 'inferior-emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;;       ;; (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
;;       (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
;;       (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
;;       (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
;;       )
;;     :config))
;; (use-package lispyville
;;   :defer t
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
;;   (add-hook 'lisp-mode-hook #'lispyville-mode)
;;   (add-hook 'org-mode-hook #'lispyville-mode)
;;   :config
;;   (lispyville-set-key-theme '(operators
;;                               c-w
;;                               (escape insert)
;;                               (additional-movement normal visual motion)
;;                               slurp/barf-lispy
;;                               ;additional
;;                               mark))
;;   (lispy-define-key lispy-mode-map "v" #'lispyville-toggle-mark-type) )
;; (with-eval-after-load 'lispyville
;;   (lispyville-set-key-theme
;;    '(operators
;;      c-w
;;      (escape insert)
;;      (additional-movement normal visual motion))))
(with-eval-after-load 'lispyville
  ;; TODO: lispy-occur: helm-occur does not restrict to region.
  (lispyville-set-key-theme
   '(operators            ; Add equivalent for lispy-delete?
     c-w                  ; Bind M-backspace to lispyville-delete-backward-word?
     (escape insert)
     slurp/barf-cp
     ;; (mark insert)
     mark-toggle                        ; TODO: Check out readme.
     ))
  (lispyville--define-key '(motion normal visual)
    (kbd "M-h") #'lispyville-previous-opening
    (kbd "M-l") #'lispyville-next-opening
    (kbd "M-j") #'lispy-down
    (kbd "M-k") #'lispy-up
    (kbd "M-H") #'lispy-up-slurp        ; lispy-down-slurp?
    (kbd "M-J") #'lispyville-drag-forward
    (kbd "M-K") #'lispyville-drag-backward
    (kbd "M-L") #'lispy-move-right      ; lispy-up-slurp?
    (kbd "C-x C-e") #'lispy-eval
    (kbd "C-j") #'lispy-split
    (kbd "C-1") #'lispy-describe-inline
    (kbd "C-2") #'lispy-arglist-inline
    (kbd "C-4") #'lispy-x
    ;; (kbd "M-;") #'lispy-comment ; This conflicts with `iedit-toggle-selection' default binding.
    ;; TODO: lispy-eval-and-replace
    ")" #'lispy-right-nostring
    (kbd "=") #'lispyville-prettify)
  (lispyville--define-key 'insert
    ";" 'lispy-comment
    ":" 'lispy-colon
    "'" 'lispy-tick
    "`" 'lispy-backtick
    "\"" 'lispy-quotes
    "(" 'lispy-parens
    ")" 'lispy-right-nostring)
  (lispyville--define-key '(motion normal)
    "q" 'lispy-ace-paren              ; REVIEW: Conflicts with magit-blame's quit.  Fixed?
    ;; "Q" 'lispy-ace-symbol
    "Y" 'lispy-new-copy
    "C" 'lispy-clone
    "D" 'lispy-kill))
(provide 'init-lispyville)
