(local-require 'targets)
(use-package targets
  :load-path "~/.emacs.d/site-lisp/targets.el"
  :init
  (setq targets-user-text-objects '((pipe "|" nil separator)
                                    (paren "(" ")" pair :more-keys "b")
                                    (bracket "[" "]" pair :more-keys "r")
                                    (curly "{" "}" pair :more-keys "c")))
  :config
  (targets-setup t
                 :inside-key nil
                 :around-key nil
                 :remote-key nil))
(provide 'init-targets)
