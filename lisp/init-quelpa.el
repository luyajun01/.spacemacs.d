(use-package quelpa
  :ensure t
  :config
  (setq quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :ensure t
  :after quelpa)