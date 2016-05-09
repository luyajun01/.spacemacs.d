;; @see https://github.com/abo-abo/hydra

;; use similar key bindings as init-evil.el
(defhydra hydra-launcher (:color blue)
  "Launch"
  ("mq" lookup-doc-in-man "man")
  ("hm" counsel-bookmark-goto "Bookmark")
  ("rr" counsel-recentf-goto "Recent files")
  ("ss" wg-create-workgroup "New window layout")
  ("ll" my-wg-switch-workgroup "Load window layout")
  ("tr" ansi-term "ansi-term")
  ("gg" 'w3m-google-search "Search")
  ("gf" 'w3m-google-by-filetype "Search by filetype")
  ("gd" 'w3m-search-financial-dictionary "Financial dictionary")
  ("gq" 'w3m-stackoverflow-search "StackOverflow")
  ("gj" 'w3m-search-js-api-mdn "Javascript API")
  ("ga" 'w3m-java-search "Java")
  ("gh" 'w3mext-hacker-search "Code search")
  ("db" sdcv-search-pointer "Stardict in buffer")
  ("dt" sdcv-search-input+  "Stardict in tooltip")
  ("dd" my-lookup-dict-org "Lookup dict.org")
  ("dw" define-word "Lookup word")
  ("dp" define-word-at-point "Lookup word at point")
  ("q" nil "cancel"))
;; Because in message-mode/article-mode we've already use `y' as hotkey
(global-set-key (kbd "C-c C-y") 'hydra-launcher/body)

;; {{ mail
;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
;; gnus-group-mode
(eval-after-load 'gnus-group
  '(progn
     (defhydra hydra-gnus-group (:color blue)
       "Do?"
       ("a" gnus-group-list-active "REMOTE groups A A")
       ("l" gnus-group-list-all-groups "LOCAL groups L")
       ("c" gnus-topic-catchup-articles "Read all c")
       ("G" gnus-group-make-nnir-group "Search server G G")
       ("g" gnus-group-get-new-news "Refresh g")
       ("s" gnus-group-enter-server-mode "Servers")
       ("m" gnus-group-new-mail "Compose m OR C-x m")
       ("#" gnus-topic-mark-topic "mark #")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)))

;; gnus-summary-mode
(eval-after-load 'gnus-sum
  '(progn
     (defhydra hydra-gnus-summary (:color blue)
       "Do?"
       ("n" gnus-summary-insert-new-articles "Refresh / N")
       ("f" gnus-summary-mail-forward "Forward C-c C-f")
       ("!" gnus-summary-tick-article-forward "Mail -> disk !")
       ("p" gnus-summary-put-mark-as-read "Mail <- disk")
       ("c" gnus-summary-catchup-and-exit "Read all c")
       ("e" gnus-summary-resend-message-edit "Resend S D e")
       ("R" gnus-summary-reply-with-original "Reply with original R")
       ("r" gnus-summary-reply "Reply r")
       ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
       ("w" gnus-summary-wide-reply "Reply all S w")
       ("#" gnus-topic-mark-topic "mark #")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue)
       "Do?"
       ("f" gnus-summary-mail-forward "Forward")
       ("R" gnus-article-reply-with-original "Reply with original R")
       ("r" gnus-article-reply "Reply r")
       ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
       ("o" gnus-mime-save-part "Save attachment at point o")
       ("w" gnus-article-wide-reply "Reply all S w")
       ("v" w3mext-open-with-mplayer "Open video/audio at point")
       ("d" w3mext-download-rss-stream "Create shell command to download stream")
       ("b" w3mext-open-link-or-image-or-url "Open link under cursor or page URL with external browser")
       ("f" w3m-lnum-follow "Click link/button/input")
       ("F" w3m-lnum-goto "Move focus to link/button/input")
       ("q" nil "cancel"))
     ;; y is not used by default
     (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

;; message-mode
(eval-after-load 'message
  '(progn
     (defhydra hydra-message (:color blue)
       "Do?"
       ("ca" mml-attach-file "Attach C-c C-a")
       ("cc" message-send-and-exit "Send C-c C-c")
       ("q" nil "cancel"))))
(defun message-mode-hook-setup ()
  (local-set-key (kbd "C-c C-y") 'hydra-message/body)
  (local-set-key (kbd "M-;") 'comment-dwim))
(add-hook 'message-mode-hook 'message-mode-hook-setup)
;; }}

(provide 'init-hydra)
;;; init-hydra.el ends here
