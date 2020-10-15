;;posframe
(add-to-list 'load-path "~/.spacemacs.d/private/posframe")
(require 'posframe)
(ivy-posframe-mode 1)
(company-posframe-mode 1)
;;snails
(add-to-list 'load-path "~/.spacemacs.d/private/snails")
(require 'snails)

;;company-mode-add-digit
(defun ora-company-number ()
  "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k)))))

(let ((map company-active-map))
  (mapc
   (lambda (x)
     (define-key map (format "%d" x) 'ora-company-number))
   (number-sequence 0 9))
  (define-key map " " (lambda ()
                        (interactive)
                        (company-abort)
                        (self-insert-command 1)))
  (define-key map (kbd "<return>") nil))

;;motion
;; e ,r 移动
(define-key evil-normal-state-map "e" 'evil-forward-symbol-begin)
(define-key evil-normal-state-map "r" 'evil-forward-symbol-end)
;; (define-key evil-normal-state-map "E" 'evil-forward-symbol-end)
(define-key evil-normal-state-map "b" 'evil-backward-symbol-begin)
;; (define-key evil-normal-state-map ";" 'evil-repeat-find-char-or-evil-backward-symbol-begin)
(define-key evil-normal-state-map "R" 'evil-backward-symbol-end)

(define-key evil-visual-state-map "e" 'evil-forward-symbol-begin)
(define-key evil-visual-state-map "r" 'evil-forward-symbol-end)
;; (define-key evil-visual-state-map "E" 'evil-forward-symbol-end)
(define-key evil-visual-state-map "b" 'evil-backward-symbol-begin)
(define-key evil-visual-state-map "R" 'evil-backward-symbol-end)


;; de dr
(define-key evil-motion-state-map "e" 'evil-forward-symbol-end)
(define-key evil-motion-state-map "r" 'evil-backward-symbol-begin)
;; dae die
(define-key evil-outer-text-objects-map "e" 'evil-a-symbol)
(define-key evil-inner-text-objects-map "e" 'evil-inner-symbol)


;;;###autoload
(evil-define-motion evil-forward-symbol-begin(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (evil-forward-beginning 'evil-symbol count)
  (let ((sym (thing-at-point 'evil-symbol)))
    (while (and sym (not (string-match "\\<" sym)))
      (evil-forward-beginning 'evil-symbol 1)
      (setq sym (thing-at-point 'evil-symbol))
      )
    )
  )

;;;###autoload
(evil-define-motion evil-backward-symbol-begin(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  ;; (evil-signal-at-bob-or-eob count)
  ;; (forward-evil-symbol count)
  (evil-backward-beginning 'evil-symbol count)
  (let ((sym (thing-at-point 'evil-symbol)))
    (while (and sym (not (string-match "\\<" sym)))
      (evil-backward-beginning 'evil-symbol 1)
      (setq sym (thing-at-point 'evil-symbol)))))


;;;###autoload
(evil-define-motion evil-forward-symbol-end(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (forward-evil-symbol count)

  ;; (let ((sym (thing-at-point 'evil-symbol)))
  ;;   (while (and sym (not (string-match "^\\<" sym)))
  ;;     (evil-forward-end 'evil-symbol 1)
  ;;     (setq sym (thing-at-point 'evil-symbol))
  ;;     )
  ;;   )
  )

;;;###autoload
(evil-define-motion evil-backward-symbol-end(count)
  "Move to the end of the COUNT-th next symbol."
  ;; :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (evil-backward-end 'symbol count)
  (let ((sym (thing-at-point 'evil-symbol)))
    (while (and sym (not (string-match "\\<" sym)))
      (evil-backward-end 'evil-symbol 1)
      (setq sym (thing-at-point 'evil-symbol))
      )
    )
)

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

;;lazy-set-key
(add-to-list 'load-path "~/.spacemacs.d/private/lazy-set-key")
(require 'lazy-set-key)

;;lazy-load
(add-to-list 'load-path "~/.spacemacs.d/private/lazy-load")
(require 'lazy-load)

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
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
(add-hook 'python-mode-hook #'rainbow-delimiters-mode)

;; ov-hight
(add-to-list 'load-path "~/.spacemacs.d/private/ov-highlight") ; add auto-save to your load-path
(require 'ov-highlight)
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

;;awesome-pair
(add-to-list 'load-path "~/.spacemacs.d/private/awesome-pair") ; add auto-save to your load-path
(require 'awesome-pair)
(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'org-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               ;; 'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               ;; 'php-mode-hook
               'python-mode-hook
               ;; 'js-mode-hook
               ;; 'go-mode-hook
               ;; 'qml-mode-hook
               ;; 'jade-mode-hook
               ;; 'css-mode-hook
               ;; 'ruby-mode-hook
               ;; 'coffee-mode-hook
               ;; 'rust-mode-hook
               ;; 'qmake-mode-hook
               ;; 'lua-mode-hook
               ;; 'swift-mode-hook
               'minibuffer-inactive-mode-hook
               ))

  (add-hook hook '(lambda () (awesome-pair-mode 1))))

(define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
(define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
(define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
(define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
(define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
(define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
(define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

(define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
(define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

(define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

(define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
(define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
(define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

(define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
(define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
(define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
(define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
(define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

(define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
(define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
(define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

;;org-download
(add-to-list 'load-path "~/.spacemacs.d/private/org-download") ; add auto-save to your load-path
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

;; pyim
;; (use-package pyim
;;   :ensure nil
;;   :demand t
;;   :config
;;   ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
;;   (use-package pyim-basedict
;;     :ensure nil
;;     :config (pyim-basedict-enable))

;;   (setq default-input-method "pyim")

;;   ;; 我使用全拼
;;   (setq pyim-default-scheme 'pyim-shuangpin)

;;   ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;;   ;; 我自己使用的中英文动态切换规则是：
;;   ;; 1. 光标只有在注释里面时，才可以输入中文。
;;   ;; 2. 光标前是汉字字符时，才能输入中文。
;;   ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-dynamic-english
;;                   pyim-probe-isearch-mode
;;                   pyim-probe-program-mode
;;                   pyim-probe-org-structure-template))

;;   (setq-default pyim-punctuation-half-width-functions
;;                 '(pyim-probe-punctuation-line-beginning
;;                   pyim-probe-punctuation-after-punctuation))

;;   ;; 开启拼音搜索功能
;;   (pyim-isearch-mode 1)

;;   ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
;;   ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
;;   ;; 手动安装 posframe 包。
;;   (setq pyim-page-tooltip 'posframe)

;;   ;; 选词框显示5个候选词
;;   (setq pyim-page-length 5)

;;   :bind
;;   (("C-S-P" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
;;    ("C-;" . pyim-delete-word-from-personal-buffer)))


;; deprecated
;; (defun zilongshanren-misc/post-init-pyim ()
;;   (progn
;;     ;; use librime as wubi input
;;     ;; 参考这个设置 pyim 使用 liberime 库 https://emacs-china.org/t/mac-emacs-rime/ 需要emacs 26的dynamic module功能
;;     ;; 设置极点五笔可以参考https://github.com/zilongshanren/rime-wubi86-jidian
;;     (eval-and-compile
;;       (if (fboundp 'window-inside-edges)
;;           ;; Emacs devel.
;;           (defalias 'th-window-edges
;;             'window-inside-edges)
;;         ;; Emacs 21
;;         (defalias 'th-window-edges
;;           'window-edges)
;;         ))

;;     (defun th-point-position ()
;;       "Return the location of POINT as positioned on the selected frame.
;;   Return a cons cell (X . Y)"
;;       (let* ((w (selected-window))
;;              (f (selected-frame))
;;              (edges (th-window-edges w))
;;              (col (current-column))
;;              (row (count-lines (window-start w) (point)))
;;              (x (+ (car edges) col))
;;              (y (+ (car (cdr edges)) row)))
;;         (cons x y)))


;;     (defun display-current-input-method-title (arg1 &optional arg2 arg3)
;;       "display current input method name"
;;       (when current-input-method-title
;;         (set-mouse-position (selected-frame) (car (th-point-position)) (cdr (th-point-position)))
;;         (x-show-tip current-input-method-title (selected-frame) nil 1  20 -30)))

;;     (advice-add 'evil-insert :after 'display-current-input-method-title)

;;     (when (functionp 'module-load)
;;       (progn
;;         (setq load-path (cons (file-truename "~/.spacemacs.d/") load-path))
;; (require 'pyim)
;;         (require 'liberime nil t)
;;         (require 'posframe)
;;         ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
;;         (use-package pyim-basedict
;;           :ensure nil
;;           :config (pyim-basedict-enable))
;;         (setq default-input-method "pyim")
;;         (setq pyim-page-tooltip 'posframe)
;;         (setq pyim-page-length 9)
;;         ;; (setq-default pyim-english-input-switch-functions
;;         ;;               '(pyim-probe-program-mode
;;         ;;                 ;; pyim-probe-auto-english
;;         ;;                 pyim-probe-org-structure-template))
;;         (setq-default pyim-english-input-switch-functions
;;                       '(pyim-probe-dynamic-english
;;                         pyim-probe-isearch-mode
;;                         pyim-probe-program-mode
;;                         pyim-probe-org-structure-template))
;;         ;; (setq pyim-page-tooltip 'minibuffer) ;展示方式为minibuffer mode
;;         ;; 开启拼音搜索功能
;;         (pyim-isearch-mode 1)
;;         (setq-default pyim-punctuation-half-width-functions
;;                       '(pyim-probe-punctuation-line-beginning
;;                         pyim-probe-punctuation-after-punctuation))
;;         ;; 不用频率切换输入法了。这个东西太好使了
;;         (bind-key* "C-S-p" 'pyim-convert-code-at-point)
;;         (liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport" (file-truename "~/Library/Rime"))
;;         ;; 使用这个来查看当前输入法有哪些，不错
;;         (liberime-get-schema-list)
;;         ;;(liberime-select-schema "double_pinyin_flypy_fluency")
;;         ;(liberime-select-schema "xhup_fluency")
;;         (setq pyim-default-scheme 'pyim-shuangpin)))))

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
  (setq pyim-default-scheme 'xiaohe-shuangpin)

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
  (setq pyim-page-tooltip 'posframe)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 6)

  :bind
  (
   ("C-S-p" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
   ;; ("C-;" . pyim-delete-word-from-personal-buffer)
   )
  )
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

;;awesome-tray
(add-to-list 'load-path "~/.spacemacs.d/private/awesome-tray")
(require 'awesome-tray)

;;google-translate
;; (add-to-list 'load-path "~/.spacemacs.d/private/google-translate") ; add color-rg to your load-path
;; (require 'google-translate)

;;multi-translate
;; (add-to-list 'load-path "~/.spacemacs.d/private/multi-translate.el") ; add color-rg to your load-path
;; (require 'multi-translate)

;;org-wiki
(add-to-list 'load-path "~/.spacemacs.d/private/org-wiki") ; add color-rg to your load-path
(require 'org-wiki)

;;org-noter
(add-to-list 'load-path "~/.spacemacs.d/private/org-noter") ; add color-rg to your load-path
(require 'org-noter)

;;python
(defun my-python-line ()
  (interactive)
  (save-excursion
    (setq the_script_buffer (format (buffer-name)))
    (end-of-line)
    (kill-region (point) (progn (back-to-indentation) (point)))
    (if  (get-buffer  "*Python*")
        (message "")
      (run-python "ipython" nil nil))
    ;; (setq the_py_buffer (format "*Python[%s]*" (buffer-file-name)))
    (setq the_py_buffer "*Python*")
    (switch-to-buffer-other-window  the_py_buffer)
    (goto-char (buffer-end 1))
    (yank)
    (comint-send-input)
    (switch-to-buffer-other-window the_script_buffer)
    (yank))
  (end-of-line)
  (next-line)
  )

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map "\C-l" 'my-python-line)
            ))

(setq-default electric-indent-inhibit t)

;;paredit-mode
;; (add-hook 'prog-mode-hook 'paredit-mode)
;; (add-hook 'python-mode-hook 'paredit-mode)
;; (add-hook 'org-mode-hook 'paredit-mode)
;; ;;mwe-log-commands
;; (add-to-list 'load-path "~/.spacemacs.d/private/mwe-log-commands")
;; (require 'mwe-log-commands)
;; ;;shell-command-extension
;; (add-to-list 'load-path "~/.spacemacs.d/private/shell-command-extension")
;; (require 'shell-command-extension)
;; ;;basic-toolkit
;; (add-to-list 'load-path "~/.spacemacs.d/private/basic-toolkit")
;; (require 'basic-toolkit)
;; ;;lazycat-toolkit
;; (add-to-list 'load-path "~/.spacemacs.d/private/lazycat-toolkit")
;; (require 'lazycat-toolkit)
;; ;;paredit-extension
;; (add-to-list 'load-path "~/.spacemacs.d/private/paredit-extension")
;; (require 'paredit-extension)

;;高亮当前行
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;;hide{} 内容
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
              ("C-c TAB" . hs-toggle-hiding)
              ("M-+" . hs-show-all))
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-special-modes-alist
   (mapcar 'purecopy
           '((c-mode "{" "}" "/[*/]" nil nil)
             (c++-mode "{" "}" "/[*/]" nil nil)
             (rust-mode "{" "}" "/[*/]" nil nil)))))

;;显示空白
(use-package whitespace
  :hook (after-init . global-whitespace-mode)
  :config
  ;; Don't use different background for tabs.
  (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))
  ;; Only use background and underline for long lines, so we can still have
  ;; syntax highlight.

  ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
  ;; is it's due to the variables with the same name as the faces in
  ;; whitespace.el.  Anyway, we have to manually set some attribute to
  ;; unspecified here.
  (face-spec-set 'whitespace-line
                 '((((background light))
                    :background "#d8d8d8" :foreground unspecified
                    :underline t :weight unspecified)
                   (t
                    :background "#404040" :foreground unspecified
                    :underline t :weight unspecified)))

  ;; Use softer visual cue for space before tabs.
  (face-spec-set 'whitespace-space-before-tab
                 '((((background light))
                    :background "#d8d8d8" :foreground "#de4da1")
                 (t
                    :inherit warning
                    :background "#404040" :foreground "#ee6aa7")))

  (setq
   whitespace-line-column nil
   whitespace-style
   '(face             ; visualize things below:
     empty            ; empty lines at beginning/end of buffer
     lines-tail       ; lines go beyond `fill-column'
     space-before-tab ; spaces before tab
     trailing         ; trailing blanks
     tabs             ; tabs (show by face)
     tab-mark         ; tabs (show by symbol)
     )))


;; (use-package so-long
;;   :ensure nil
;;   :config (global-so-long-mode 1))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;;isolate
(add-to-list 'load-path "~/.spacemacs.d/private/isolate")
(require 'isolate)

;;evil-easymotion
(add-to-list 'load-path "~/.spacemacs.d/private/evil-easymotion")
(require 'evil-easymotion)

;; Globally
(evil-snipe-override-mode 1)
(define-key evil-snipe-parent-transient-map (kbd "C-,")
  (evilem-create 'evil-snipe-repeat
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))

;;thingatpt
(add-to-list 'load-path "~/.spacemacs.d/private/thingatpt")
(require 'thingatpt)

;;thing-edit
(add-to-list 'load-path "~/.spacemacs.d/private/thing-edit")
(require 'thing-edit)

;;one-key
(add-to-list 'load-path "~/.spacemacs.d/private/one-key")
(require 'one-key)
;;lispy
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'python-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'ipython-mode-hook (lambda () (lispy-mode 1)))

;;org-ref
(require 'org-ref)
(setq bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
      bibtex-completion-library-path "~/Dropbox/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "~/Dropbox/bibliography/helm-bibtex-notes")

;;ebib
(require 'general)
(use-package ebib
  :general
  ([f5] 'ebib)
  (ebib-multiline-mode-map
   "C-c C-c" 'ebib-quit-multiline-buffer-and-save
   "C-c C-q" 'ebib-cancel-multiline-buffer
   "C-c C-s" 'ebib-save-from-multiline-buffer)
  :custom
  (bibtex-autokey-name-case-convert-function 'capitalize)
  (bibtex-autokey-titlewords 0)
  (bibtex-autokey-year-length 4)
  (ebib-uniquify-keys t)
  (ebib-bibtex-dialect 'biblatex)
  (ebib-index-window-size 10)
  (ebib-preload-bib-files '("~/Dropbox/bibliography/references.bib"))
  (ebib-notes-use-single-file "~/Dropbox/Bibliography/Notes.org")
  (ebib-file-search-dirs '("~/Dropbox/Bibliography/bibtex-pdfs/"))
  (ebib-reading-list-file "~/Dropbox/Bibliography/ReadingList.org")
  (ebib-keywords-file "~/Dropbox/Bibliography/ebib-keywords.txt")
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-file-save-on-exit 'always)
  (ebib-file-associations '(("pdf")) "using Emacs to open pdf")
  (ebib-use-timestamp t "recording the time that entries are added")
  (ebib-index-columns '(("Entry Key" 20 t)
			            ("Author/Editor" 40 nil)
			            ("Year" 6 t)
			            ("Title" 50 t)))
  (ebib-index-default-sort '("timestamp" . descend)))

(require 'init-hydra)
(use-package org-ref
  :general
  (z-spc-leader-def "r" 'org-ref-hydra/body)
  :pretty-hydra
  ((:color red :quit-key "q")
   ("Insert"
    (("i" org-ref-helm-insert-cite-link "citation key")
     ("r" org-ref-helm-insert-ref-link "ref link")
     ("l" org-ref-helm-insert-label-link "label link"))
    "Browse"
    (("b" helm-bibtex "bibtex")
     ("s" crossref-lookup "lookup"))
    "Add"
    (("a" crossref-add-bibtex-entry "new entry")
     ("d" doi-add-bibtex-entry "doi"))))
  :custom
  (bibtex-dialect 'biblatex)
  (org-ref-bibliography-notes "~/Dropbox/bibliography/helm-bibtex-notes")
  (org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib"))
  (org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")
  (org-ref-show-broken-links nil)
  (org-ref-default-ref-type "eqref")
  (org-ref-default-citation-link "citet")
  :config
  (require 'org-ref-citeproc)
  (defun org-ref-grep-pdf (&optional _candidate)
    "Search pdf files of marked CANDIDATEs."
    (interactive)
    (let ((keys (helm-marked-candidates))
	  (get-pdf-function org-ref-get-pdf-filename-function))
      (helm-do-pdfgrep-1
       (-remove (lambda (pdf)
		  (string= pdf ""))
		(mapcar (lambda (key)
			  (funcall get-pdf-function key))
			keys)))))
  (helm-add-action-to-source "Grep PDF" 'org-ref-grep-pdf helm-source-bibtex 1)

  (setq helm-bibtex-map
	(let ((map (make-sparse-keymap)))
	  (set-keymap-parent map helm-map)
	  (define-key map (kbd "C-s") (lambda () (interactive)
					(helm-run-after-exit 'org-ref-grep-pdf)))
	  map))
  (push `(keymap . ,helm-bibtex-map) helm-source-bibtex))


;;org-mode
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

  (use-package company-math
    :ensure t)

  (use-package company-auctex
    :ensure t
    :config (progn
              (defun company-auctex-labels (command &optional arg &rest ignored)
                "company-auctex-labels backend"
                (interactive (list 'interactive))
                (case command
                  (interactive (company-begin-backend 'company-auctex-labels))
                  (prefix (company-auctex-prefix "\\\\.*ref{\\([^}]*\\)\\="))
                  (candidates (company-auctex-label-candidates arg))))

              (add-to-list 'company-backends
                           '(company-auctex-macros
                             company-auctex-environments
                             company-math-symbols-unicode
                             company-math-symbols-latex))

              (add-to-list 'company-backends #'company-auctex-labels)
              (add-to-list 'company-backends #'company-auctex-bibs)))

  (add-hook 'org-mode-hook 'company-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-math-symbols-unicode company-math-symbols-latex company-files company-en-words company-dabbrev)))
            )

;;换行符
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b01110000
   #b00010000
   #b00010000
   #b00000000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00001000
   #b00001000
   #b00001110
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;;ivy-posframe-style
(require 'ivy-posframe)
;; display at `ivy-posframe-style'
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
;; Different command can use different display function.
(setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
;; Different command can use different display function.
(setq ivy-posframe-display-functions-alist
      '((swiper          . ivy-posframe-display-at-frame-center)
        (counsel-recentf . ivy-posframe-display-at-window-center)
        (counsel-M-x     . ivy-posframe-display-at-window-center)
        (t               . ivy-posframe-display)))
(ivy-posframe-mode 1)

;; org-brain
(add-to-list 'load-path "~/.spacemacs.d/private/org-brain")
(require 'org-brain)

;; (use-package org-brain
;;   :ensure t
;;   :init
;;   (setq org-brain-path "~/Documents/坚果云/我的坚果云/github/wiki")
;;   ;; For Evil users
;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
;;   :config
;;   (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
;;   ;; (setq org-id-track-globally t)
;;   ;; (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
;;   ;; (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
;;   ;; (push '("b" "Brain" plain (function org-brain-goto-end)
;;   ;;         "* %i%?" :empty-lines 1)
;;   ;;       org-capture-templates)
;;   (setq org-brain-visualize-default-choices 'all)
;;   (setq org-brain-title-max-length 12)
;;   (setq org-brain-include-file-entries nil
;;         org-brain-file-entries-use-title nil))

;; Allows you to edit entries directly from org-brain-visualize
(use-package polymode
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))


     ;; dired-mode
    ;; ;; ;;返回上层目录，我绑定了快捷键i, 特别好按，非常流畅
    (add-hook 'dired-mode-hook
               (lambda ()
                 (define-key dired-mode-map (kbd "i")
                   (lambda () (interactive) (find-alternate-file "..")))))

;;; R related modes
    (use-package polymode
      :mode
      (("\\.Rmd" . poly-markdown+r-mode))
      :init
      (autoload 'r-mode "ess-site.el" "Major mode for editing R source." t)
      :defer t
      )
    (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode))
    (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Snw$" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rnw$" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rmd$" .  poly-markdown+r-mode))
    (add-to-list 'auto-mode-alist '("\\.rapport$" . poly-rapport-mode))
    (add-to-list 'auto-mode-alist '("\\.Rhtml$" . poly-html+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rbrew$" . poly-brew+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rcpp$" . poly-r+c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cppR$" . poly-c++r-mode))
     (defun ess/init-polymode ()
       (use-package poly-R
         :defer t)
       (use-package poly-markdown
         :defer t)
       (use-package poly-markdown+r
         :defer t)
       (use-package poly-noweb+r
         :defer t)
       )


(provide 'init-private)
