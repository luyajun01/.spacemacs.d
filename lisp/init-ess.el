;;  ESS (Emacs Speaks Statistics)
  (with-eval-after-load 'ess
    ;; (ess-set-style 'RStudio)
    ;; (setq ess-offset-arguments 'prev-line)
    ;;一定要记得加company-yasnipet
    (defun my-ess-config ()
      (make-variable-buffer-local 'company-backends)
      (add-to-list 'company-backends
                   '(company-R-args company-yasnippet company-tabnine company-R-library  company-R-objects company-dabbrev-code :separate))
      )
    (add-hook 'ess-mode-hook #'my-ess-config)
                                        ; (add-hook 'find-file-hook 'my-r-style-hook)
                                        ;  ;;;pipe function %>% key
                                        ; (defun my-r-style-hook ()
    ;;   (when (string-match (file-name-extension buffer-file-name) "[r|R]$")
    ;;     (ess-set-style 'RStudio)
    ;;     )
    ;;   )
    ;; ;;highlight color code
    ;; (add-hook 'ess-mode-hook
    ;;           '(lambda()
    ;;              (font-lock-add-keywords
    ;;               nil
    ;;               '(                  ("\\<\\(if\\|for\\|function\\|return\\|$\\|@\\)\\>[\n[:blank:]]*(" 1
    ;;                                    font-lock-keyword-face) ; must go first to override highlighting below
    ;;                                   ("\\<\\([.A-Za-z][._A-Za-z0-9]*\\)[\n[:blank:]]*(" 1
    ;;                                    font-lock-function-name-face) ; highlight function names
    ;;                                   ("\\([(,]\\|[\n[:blank:]]*\\)\\([.A-Za-z][._A-Za-z0-9]*\\)[\n[:blank:]]*=[^=]"
    ;;                                    2 font-lock-reference-face)
    ;;                                   ;; highlight numbers
    ;;                                   ("\\(-?[0-9]*\\.?[0-9]*[eE]?-?[0-9]+[iL]?\\)" 1 font-lock-type-face)
    ;;                                   ;; highlight operators
    ;;                                   ("\\(\\$\\|\\@\\|\\!\\|\\%\\|\\^\\|\\&\\|\\*\\|\(\\|\)\\|\{\\|\}\\|\\[\\|\\]\\|\\-\\|\\+\\|\=\\|\\/\\|\<\\|\>\\|:\\)" 1 font-lock-builtin-face)
    ;;                                   highlight S4 methods
    ;;                                   ("\\(setMethod\\|setGeneric\\|setGroupGeneric\\|setClass\\|setRefClass\\|setReplaceMethod\\)" 1 font-lock-reference-face)
    ;;                                   ;; highlight packages called through ::, :::
    ;;                                   ("\\(\\w+\\):\\{2,3\\}" 1 font-lock-constant-face)
    ;;                                   ))
    ;;              ))
    )
  (defun then_R_operator ()
    "%>% operator or 'then' pipe operator"
    (interactive)
    (insert " %>%")                     ; note the space before the first %
    (reindent-then-newline-and-indent))
  (global-set-key (kbd "C-S-M") 'then_R_operator)  
  ;; assign_operator
  (defun assign_R_operator ()
    "R - <- operator or 'assign' operator"
    (interactive)
    (just-one-space 1)
    (insert "<-")
    (just-one-space 1)
                                        ;(reindent-then-newline-and-indent)
    )
  (global-set-key (kbd "C-S-l") 'assign_R_operator)
  ;;bracket function () key
  (defun bracket_R_operator ()
    "R - () operator or 'bracket' operator"
    (interactive)
                                        ; (just-one-space 1)
    (insert "()")
    (backward-char 1)
                                        ;(reindent-then-newline-and-indent)
    )
  (global-set-key (kbd "C-S-b") 'bracket_R_operator)
  ;; ;;indent-region-as-r
  ;; (defun ess-indent-region-as-r (beg end)
  ;;   "Format region of code R using the R parser."
  ;;   (interactive "r")
  ;;   (let ((string (replace-regexp-in-string
  ;;                  "\"" "\\\\\\&"
  ;;                  (replace-regexp-in-string ;; how to avoid this double matching?
  ;;                   "\\\\\"" "\\\\\\&" (buffer-substring-no-properties beg end))))
  ;;         (buf (get-buffer-create "*ess-command-output*")))
  ;;     (ess-force-buffer-current "Process to load into:")
  ;;     (ess-command
  ;;      (format
  ;;       "local({oo <- options(keep.source = FALSE);
  ;; cat('\n', paste(deparse(parse(text = \"%s\")[[1L]]), collapse = '\n'), '\n', sep = '')
  ;; options(oo)})\n"
  ;;       string) buf)
  ;;     (with-current-buffer buf
  ;;       (goto-char (point-max))
  ;;       ;; (skip-chars-backward "\n")
  ;;       (let ((end (point)))
  ;;         (goto-char (point-min))
  ;;         (goto-char (1+ (point-at-eol)))
  ;;         (setq string (buffer-substring-no-properties (point) end))
  ;;         ))
  ;;     (delete-region beg end)
  ;;     (insert string)
  ;;     ))
  ;;from iqss.github.io
  ;; (add-hook 'ess-r-mode-hook
  ;;            (lambda()
  ;;              'eglot-ensure
  ;;              (make-local-variable 'company-backends)
  ;;              (delete-dups (push 'company-capf company-backends))
  ;;             (delete-dups (push 'company-files company-backends))))
  ;; Set ESS options
  ;; (setq
  ;;  ess-auto-width 'window
  ;;  ess-use-auto-complete nil
  ;;  ess-use-company 't
  ;;  ;; ess-r-package-auto-set-evaluation-env nil
  ;;  inferior-ess-same-window nil
  ;;  ess-indent-with-fancy-comments nil   ; don't indent comments
  ;;  ess-eval-visibly t                   ; enable echoing input
  ;;  ess-eval-empty t                     ; don't skip non-code lines.
  ;;  ess-ask-for-ess-directory nil        ; start R in the working directory by default
  ;;  ess-ask-for-ess-directory nil        ; start R in the working directory by default
  ;;  ess-R-font-lock-keywords             ; font-lock, but not too much
  ;;  (quote
  ;;   ((ess-R-fl-keyword:modifiers)
  ;;    (ess-R-fl-keyword:fun-defs . t)
  ;;    (ess-R-fl-keyword:keywords . t)
  ;;    (ess-R-fl-keyword:assign-ops  . t)
  ;;    (ess-R-fl-keyword:constants . 1)
  ;;    (ess-fl-keyword:fun-calls . t)
  ;;    (ess-fl-keyword:numbers)
  ;;    (ess-fl-keyword:operators . t)
  ;;    (ess-fl-keyword:delimiters)
  ;;    (ess-fl-keyword:=)
  ;;    (ess-R-fl-keyword:F&T))))
  ;; ;;formatR
  ;; From Walmes Zeviani
  ;; https://github.com/basille/.emacs.d/issues/1
  (defun ess-indent-region-with-formatr (beg end)
    "Format region of code R using formatR::tidy_source()."
    (interactive "r")
    (let ((string
           (replace-regexp-in-string
            "\"" "\\\\\\&"
            (replace-regexp-in-string ;; how to avoid this double matching?
             "\\\\\"" "\\\\\\&"
             (buffer-substring-no-properties beg end))))
	      (buf (get-buffer-create "*ess-command-output*")))
      (ess-force-buffer-current "Process to load into:")
      (ess-command
       (format                          ; R parser use 'width.cutoff = 60L'
        "local({formatR::tidy_source(text = \"\n%s\", arrow = TRUE, width.cutoff = 60L) })\n"
        string) buf)
      (with-current-buffer buf
        (goto-char (point-max))
        ;; (skip-chars-backward "\n")
        (let ((end (point)))
	      (goto-char (point-min))
	      (goto-char (1+ (point-at-eol)))
	      (setq string (buffer-substring-no-properties (point) end))
	      ))
      (delete-region beg end)
      (insert string)
      (delete-char -1)
      ))
  (defun ess-indent-region-with-formatR-tidy-source (beg end)
    "Format region of code R using formatR::tidy_source()."
    (interactive "r")
    (let ((string
           (replace-regexp-in-string
            "\"" "\\\\\\&"
            (replace-regexp-in-string ;; how to avoid this double matching?
             "\\\\\"" "\\\\\\&"
             (buffer-substring-no-properties beg end))))
          (buf (get-buffer-create "*ess-command-output*")))
      (ess-force-buffer-current "Process to load into:")
      (ess-command
       (format
        "local({
          formatR::tidy_source(text=\"\n%s\",
                               arrow=TRUE, width.cutoff=60) })\n"
        string) buf)
      (with-current-buffer buf
        (goto-char (point-max))
        ;; (skip-chars-backward "\n")
        (let ((end (point)))
          (goto-char (point-min))
          (goto-char (1+ (point-at-eol)))
          (setq string (buffer-substring-no-properties (point) end))
          ))
      (delete-region beg end)
      (insert string)
      (delete-backward-char 2)
      ))
;;styler
  (defun ess-indent-region-with-styler (beg end)
    "Format region of code R using styler::style_text()."
    (interactive "r")
    (let ((string
           (replace-regexp-in-string
            "\"" "\\\\\\&"
            (replace-regexp-in-string ;; how to avoid this double matching?
             "\\\\\"" "\\\\\\&"
             (buffer-substring-no-properties beg end))))
	      (buf (get-buffer-create "*ess-command-output*")))
      (ess-force-buffer-current "Process to load into:")
      (ess-command
       (format
        "local({options(styler.colored_print.vertical = FALSE);styler::style_text(text = \"\n%s\", reindention = styler::specify_reindention(regex_pattern = \"###\", indention = 0), indent_by = 4)})\n"
        string) buf)
      (with-current-buffer buf
        (goto-char (point-max))
        ;; (skip-chars-backward "\n")
        (let ((end (point)))
	      (goto-char (point-min))
	      (goto-char (1+ (point-at-eol)))
	      (setq string (buffer-substring-no-properties (point) end))
	      ))
      (delete-region beg end)
      (insert string)
      (delete-char -1)
      ))
(provide 'init-ess)
