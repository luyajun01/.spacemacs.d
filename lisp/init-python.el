;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2010-2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Python configurations.
;;

;;; Code:

;; Python Mode
;; Install: pip install pyflakes autopep8
(use-package python
  :ensure nil
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python"))
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  ;; Live Coding in Python
  (use-package live-py-mode)
(require 'lpy)
  (add-hook 'python-mode-hook (lambda () (lpy-mode 1)))
  )

;; (setq python-shell-interpreter "~/.virtualenvs/test/bin/python"
;;       python-shell-interpreter-args "-m IPython --simple-prompt -i")

;; (add-hook 'org-mode-hook 'company-mode)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends) '(company-anaconda company-capf)))
;;           )
(add-to-list 'company-backends 'company-ob-ipython)

(defun my-python-mode-config ()
  (setq python-indent-offset 4
        python-indent 4
        indent-tabs-mode nil
        default-tab-width 4

        ;; 设置 run-python 的参数
        python-shell-interpreter "~/.virtualenvs/test/bin/python"
        python-shell-interpreter-args "-m IPython --simple-prompt -i"
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (hs-minor-mode t)                     ;开启 hs-minor-mode 以支持代码折叠
  (auto-fill-mode 0)                    ;关闭 auto-fill-mode，拒绝自动折行
  (whitespace-mode t)                   ;开启 whitespace-mode 对制表符和行为空格高亮
  (hl-line-mode t)                      ;开启 hl-line-mode 对当前行进行高亮
  (pretty-symbols-mode t)               ;开启 pretty-symbols-mode 将 lambda 显示成希腊字符 λ
  (set (make-local-variable 'electric-indent-mode) nil))
                                        ;; 关闭自动缩进

(add-hook 'python-mode-hook 'my-python-mode-config)
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
(setq tab-width 4)
(set-variable 'python-indent-offset 4)
(set-variable 'python-indent-guess-indent-offset nil)

(use-package elpy
  :ensure t
  :config (setq python-shell-interpreter "python3")

  (put 'set-goal-column 'disabled nil)
  (elpy-enable)
  (setq elpy-rpc-ignored-buffer-size (* 1024 1024 1024))


  ;; interactive python
  (setq python-shell-interpreter-args "-i"
        elpy-rpc-python-command "python3")

  (setq python-shell-interpreter "python3"
        elpy-shell-echo-output nil
        ;; python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "python3"))


;; (setq elpy-shell-echo-output nil
;;       python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i")

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
