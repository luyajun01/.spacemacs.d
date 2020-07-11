;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Show key bind for currently entered incomplete command
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package which-key
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-bottom)))
(provide 'init-whichkey)
