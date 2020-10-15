;; Outline mode for R
(require 'company)
(require 'lazy-load)
(setq tab-always-indent 'complete)
(setq company-idle-delay 0.1)
(global-company-mode)
;; (ess-toggle-underscore nil)
(with-eval-after-load 'ess
  (setq ess-use-company t))

(add-hook 'ess-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp "^#.*----")
             (defun outline-level ()
               (cond (looking-at "^#.*----") 1)
               (t 1000)
               )

             (defun send-section-to-R ()
               (interactive ())
               (let ((beg))
                 (if (outline-on-heading-p)
                     (beginning-of-line)
                   (outline-previous-visible-heading 1))
                 (setq beg (point))
                 (set-mark (point))
                 (outline-next-visible-heading 1)
                 (previous-line 1)
                 (end-of-line 1)
                 (ess-eval-region-or-function-or-paragraph-and-step)
                 )
               )
             (local-set-key (kbd "C-c h") 'outline-hide-body)
             (local-set-key (kbd "C-c s") 'outline-show-all)
             (local-set-key (kbd "C-c <left>") 'outline-hide-entry)
             (local-set-key (kbd "C-c <right>") 'outline-show-entry)
             (local-set-key (kbd "C-c <up>") 'outline-previous-heading)
             (local-set-key (kbd "C-c <down>") 'outline-next-heading)
             (local-set-key (kbd "C-c t") 'send-section-to-R)
             )
          )

;; -------- highlight function names & keywords in R --------
(dolist (hook '(ess-mode-hook ess-r-mode-hook inferior-ess-r-mode-hook))
  (add-hook hook
            '(lambda()
               (font-lock-add-keywords
                nil
                '(("\\<\\(if\\|for\\|function\\|return\\|$\\|@\\)\\>[\n[:blank:]]*(" 1
                   font-lock-keyword-face) ; must go first to override highlighting below
                  ("\\<\\([.A-Za-z][._A-Za-z0-9]*\\)[\n[:blank:]]*(" 1
                   font-lock-function-name-face) ; highlight function names
                  ("\\([(,]\\|[\n[:blank:]]*\\)\\([.A-Za-z][._A-Za-z0-9]*\\)[\n[:blank:]]*=[^=]"
                   2 font-lock-builtin-face)
                  ;; highlight numbers
                  ("\\(-?[0-9]*\\.?[0-9]*[eE]?-?[0-9]+[iL]?\\)" 1 font-lock-constant-face)
                  ;; highlight operators
                  ("\\(\\$\\|\\@\\|\\!\\|\\%\\|\\^\\|\\&\\|\\*\\|\(\\|\)\\|\{\\|\}\\|\\[\\|\\]\\|\\-\\|\\+\\|\=\\|\\/\\|\<\\|\>\\|:\\)" 1 font-lock-type-face)
                  ;; highlight S4 methods
                  ("\\(setMethod\\|setGeneric\\|setGroupGeneric\\|setClass\\|setRefClass\\|setReplaceMethod\\)" 1 font-lock-preprocessor-face)
                  ;; highlight packages called through ::, :::
                  ("\\(\\w+\\):\\{2,3\\}" 1 font-lock-type-face)
                  )))))


(setq inferior-R-font-lock-keywords
      '((ess-S-fl-keyword:prompt . t)
        (ess-R-fl-keyword:messages . t)
        (ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:constants . t)
        (ess-fl-keyword:matrix-labels . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:= . t)
        (ess-R-fl-keyword:F&T . t)
        ;; (ess-R-fl-keyword:%op% . t)
        ))
(provide 'init-ess)
