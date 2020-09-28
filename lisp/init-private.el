;; ;;nox
;; (add-to-list 'load-path "~/.spacemacs.d/private/nox") ; add auto-save to your load-path
;; (require 'nox)
;; (dolist (hook (list
;;                'js-mode-hook
;;                'rust-mode-hook
;;                'python-mode-hook
;;                'ruby-mode-hook
;;                'java-mode-hook
;;                'sh-mode-hook
;;                'php-mode-hook
;;                'c-mode-common-hook
;;                'c-mode-hook
;;                'csharp-mode-hook
;;                'c++-mode-hook
;;                'haskell-mode-hook
;;                ))
;;   (add-hook hook '(lambda () (nox-ensure))))
;; (add-to-list 'nox-server-programs
;; 		         '((ess-r-mode inferior-ess-r-mode)
;; 		           . ("R" "--slave" "-e" "languageserver::run()")))
;; (setq nox-python-path "~/.virtualenvs/test/bin/python/")

;;quela
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; ;;lsp-pyright
(add-to-list 'load-path "~/.spacemacs.d/private/lsp-pyright")
(require 'lsp-pyright)

;; auto-save
(add-to-list 'load-path "~/.spacemacs.d/private/auto-save") ; add auto-save to your load-path
(require 'auto-save)
(auto-save-enable)

(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

;;; custom predicates if you don't want auto save.
;;; disable auto save mode when current filetype is an gpg file.
(setq auto-save-disable-predicates
      '((lambda ()
          (string-suffix-p
           "gpg"
           (file-name-extension (buffer-name)) t))))

;;ox-gfm
(add-to-list 'load-path "~/.spacemacs.d/private/ox-gfm") ; add auto-save to your load-path
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;;rainbow-delimiters
(add-to-list 'load-path "~/.spacemacs.d/private/rainbow-delimiters") ; add auto-save to your load-path
(require 'rainbow-delimiters)

;;plain-org-wiki
(add-to-list 'load-path "~/.spacemacs.d/private/plain-org-wiki") ; add auto-save to your load-path
(require 'plain-org-wiki)
(setq plain-org-wiki-directory "~/Documents/坚果云/我的坚果云/github/wiki/")

;;valign
(add-to-list 'load-path "~/.spacemacs.d/private/valign") ; add auto-save to your load-path
(require 'valign)
(add-hook 'org-mode-hook #'valign-mode)
;; (use-package valign
;;   :quelpa (valign :fetcher github :repo "casouri/valign")
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook #'valign-mode))


;;awesome-tray
(add-to-list 'load-path "~/.spacemacs.d/private/awesome-tray")
(require 'awesome-tray)

;;pyim
(use-package pyim
  :ensure nil
  :demand t
  :config
  ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))

  (setq default-input-method "pyim")

  ;; 我使用全拼
  (setq pyim-default-scheme 'pyim-shuangpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'popup)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)

  :bind
  (("C-S-P" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))

;;color-rg
(add-to-list 'load-path "~/.spacemacs.d/private/color-rg") ; add color-rg to your load-path
(require 'color-rg)

;;   ;;Chinese and English fonts alignment
(use-package cnfonts
  :config
  (cnfonts-enable)
  (setq cnfonts-use-face-font-rescale t)
  )

;;awesome-tab
(add-to-list 'load-path "~/.spacemacs.d/private/awesome-tab") ; add color-rg to your load-path
(require 'awesome-tab)
;;aweshell
(add-to-list 'load-path "~/.spacemacs.d/private/aweshell") ; add color-rg to your load-path
(require 'aweshell)
(awesome-tab-mode t)

;;evil-snipe
 (evil-snipe-override-mode 1)
;; 恢复evil的s/S，要用evil-define-key, define-key不行，a bit tricky，一个issue里抄来的
  (with-eval-after-load 'evil-snipe
    (evil-define-key* '(normal) evil-snipe-mode-map
                      "s" #'evil-substitute
                      "S" #'evil-change-whole-line)
    (define-key evil-normal-state-map "s" #'evil-substitute)
    (define-key evil-normal-state-map "S" #'evil-change-whole-line)
    )
;; 只用;来repeat，禁用移动后立即按f/t来repeat
(setq evil-snipe-repeat-keys nil)
;; override-mode之后如果要给evil-repeat绑其他键位要用evil-snipe的对应函数
  (define-key evil-normal-state-map (kbd "DEL") 'evil-snipe-repeat-reverse)
;; 不用s/S那用gs之类的吧
  (evil-define-key 'normal evil-snipe-mode-map (kbd "g s") #'evil-snipe-s)
  (evil-define-key 'normal evil-snipe-mode-map (kbd "g S") #'evil-snipe-S)
  (evil-define-key 'normal evil-snipe-mode-map (kbd "g t") #'evil-snipe-x)
  (evil-define-key 'normal evil-snipe-mode-map (kbd "g T") #'evil-snipe-X)
  (evil-define-key 'visual evil-snipe-mode-map "z" #'evil-snipe-s)
  (evil-define-key 'visual evil-snipe-mode-map "Z" #'evil-snipe-S)
  (evil-define-key 'visual evil-snipe-mode-map "x" #'evil-snipe-x)
  (evil-define-key 'visual evil-snipe-mode-map "X" #'evil-snipe-X)

;;olivetti
(require 'olivetti)


;;avy
;;行内字符移动
(defun avy-goto-word-1-backward-in-line (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-goto-word-1 char arg (point-at-bol) (point) nil))

(defun avy-goto-word-1-forward-in-line (char &optional arg)
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-goto-word-1 char arg (point) (point-at-eol) nil))

;; (use-package evil-lispy
;;   :quelpa (evil-lispy :fetcher github :repo "sp3ctum/evil-lispy")
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook #'evil-lispy-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
;;   (add-hook 'clojure-mode-hook #'evil-lispy-mode)
;;   )

(provide 'init-private)
