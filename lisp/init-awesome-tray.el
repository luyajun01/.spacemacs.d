;;awesome-tray
(require 'quelpa)
(require 'quelpa-use-package)
(use-package awesome-tray
  :quelpa (awesome-tray :fetcher github :repo "manateelazycat/awesome-tray")
  :ensure t
  :config
  (awesome-tray-mode 1)
  )

(awesome-tray-mode 1)

(provide 'init-awesome-tray)
