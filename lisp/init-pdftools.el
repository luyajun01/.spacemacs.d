;;; Require
(require 'pdf-tools)
(require 'pdf-occur)
(require 'pdf-history)
(require 'pdf-links)
(require 'pdf-outline)
(require 'pdf-annot)
(require 'pdf-sync)
(require 'lazy-set-key)
(add-to-list 'load-path "~/.spacemacs.d/private/pdf-tools-extension")
(require 'pdf-tools-extension)

;;; Code:
(pdf-tools-install)
;;;打开pdf 自动加载pdf-view-mode
(autoload 'pdf-view-mode "pdf-view" "\
Major mode in PDF buffers.

PDFView Mode is an Emacs PDF viewer.  It displays PDF files as
PNG images in Emacs buffers." t nil)

(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))

;; ;; (require 'pdf-tools)
;; ;; (package-initialize t)
;; ;; (package-activate 'pdf-tools)
;; ;; (pdf-tools-install)

;; ;;grap org-pdfview https://github.com/markus1189/org-pdfview/blob/master/org-pdfview.el
;; (add-to-list 'load-path "~/.spacemacs.d/private/org-pdfview")
;; (require 'org-pdfview)
;; ;; (eval-after-load 'org '(require 'org-pdfview))
;; (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
;; (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))

;; ;;{{{ auto revert buffer
;; ;; see @ https://github.com/politza/pdf-tools/issues/25
(setq auto-revert-interval 0.5)
(auto-revert-set-timer)
(setq revert-without-query '(".*"))
(add-hook 'pdf-view-mode-hook (lambda ()
                                (auto-revert-mode 1)
                                ))
;;}}}

;; ;; Avoid hanging Emacs by closing some minor mode
(defun fg/pdf-view-mode-hook ()
  (company-mode -1)
  ;; (auto-complete-mode -1)
  (linum-mode -1)
  (pyim-isearch-mode -1)
  (yas-minor-mode -1)
  (pangu-spacing-mode -1)
  (aggressive-indent-mode -1)
  (line-number-mode -1)
  (font-lock-mode -1)
  (column-number-mode -1)
  (cua-mode -1)
  ;; (eyebrose-mode -1)
  ;; (auto-revert-mode -1)
  (symbol-overlay-mode -1)
  ;; (folding-mode -1)
  )

;; Close evil-mode in pdf-tools
;; ;; to see @ https://github.com/politza/pdf-tools/issues/201
(evil-set-initial-state 'pdf-view-mode 'emacs)

(add-hook 'pdf-view-mode-hook 'fg/pdf-view-mode-hook)

;; (add-to-list 'display-buffer-alist '("\\.pdf\\(<[^>]+>\\)?$" . (display-buffer-pop-up-window-pdf-split-horizontally)))

;; ;; pdf-tools keybindings
;; ;; to see @ https://github.com/abo-abo/hydra/wiki/PDF-Tools
;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install)
;;   (setq-default pdf-view-display-size 'fit-page)
;;   (setq mouse-drag-copy-region t)
;;   (bind-keys :map pdf-view-mode-map
;;              ("\\" . hydra-pdftools/body)
;;              ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
;;              ("l"  . image-forward-hscroll)
;;              ("h"  . image-backward-hscroll)
;;              ("e"  . pdf-view-goto-page)
;;              ("d"  . pdf-view-next-page-command)
;;              ("u"  . pdf-view-previous-page-command)
;;              ;; quit
;;              ("q" . quit-window)
;;              ("Q" . kill-this-buffer)
;;              ("ZQ" . kill-this-buffer)
;;              ("ZZ" . quit-window)
;;              ("]]" . pdf-view-next-page-command)
;;              ("[[" . pdf-view-previous-page-command)
;;              )
;;   ;; (use-package org-pdfview
;;   ;;   :ensure t)
;;   )

;; pdf-tool setting using hydra
(defhydra hydra-pdftools (:color blue :hint nil)
  "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_gg_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y"  pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :co(setq helm-bibtex-pdf-open-function
                                (lambda (fpath)
                                  (call-process "pdf-tools" nil 0 nil fpath)))lor red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-dark-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("gg" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red))
;; (global-set-key (kbd "<f5>") 'hydra-pdftools/body)
(define-key pdf-view-mode-map (kbd "<f5>") 'hydra-pdftools/body)

;;{{{ remove some keybinding and define new keybinding in pdf-tools
(define-key pdf-view-mode-map (kbd "SPC") nil)
(define-key pdf-view-mode-map (kbd "SPC hb") 'helm-bibtex)
(define-key pdf-view-mode-map (kbd "SPC w") 'hydra-window/body)
(define-key pdf-view-mode-map (kbd "SPC oy") 'xah-copy-file-path)
(define-key pdf-view-mode-map (kbd "SPC ii") 'ibuffer)
(define-key pdf-view-mode-map (kbd "SPC ff") 'find-file)
(define-key pdf-view-mode-map (kbd "SPC bb") 'switch-to-buffer)
(define-key pdf-view-mode-map (kbd "SPC bp") 'previous-buffer)
(define-key pdf-view-mode-map (kbd "SPC bn") 'next-buffer)
;; window movement like vim style
(define-key pdf-view-mode-map (kbd "C-w j") 'evil-window-down)
(define-key pdf-view-mode-map (kbd "C-w k") 'evil-window-up)
(define-key pdf-view-mode-map (kbd "C-w h") 'evil-window-left)
(define-key pdf-view-mode-map (kbd "C-w l") 'evil-window-right)
;; M-x
(define-key pdf-view-mode-map (kbd "SPC SPC") 'counsel-M-x)
;; buffer
(define-key pdf-view-mode-map (kbd "SPC b p") 'previous-buffer)
(define-key pdf-view-mode-map (kbd "SPC b n") 'next-buffer)
(define-key pdf-view-mode-map (kbd "SPC TAB") 'spacemacs/alternate-buffer)
;; quickly open my files
(define-key pdf-view-mode-map (kbd "SPC a") 'hydra-fgfiles/body)
;;;}}}
(define-key pdf-view-mode-map (kbd "SPC [") 'hydra-tab/body)
(define-key pdf-view-mode-map (kbd "SPC ]") 'hydra-workgroups/body)
;; some useful keybinding
(define-key pdf-view-mode-map (kbd "C-c C-g") 'pdf-sync-forward-search)
;; execture
(define-key pdf-view-mode-map (kbd "M-!") 'execute-program)

;; more fine-grained zooming
(setq pdf-view-resize-factor 1.1)

;; ;; pdf-view-mode for goldendict
;; (require 'goldendict)
;; (evil-leader/set-key (kbd "og") 'goldendict-dwim)

;; ;;interleave
(defun my-interleave-hook ()
  (with-current-buffer interleave-org-buffer
    ;; Do something meaningful here
    (message "Hi there. I'm in the org buffer!")))

(add-hook 'interleave-mode-hook #'my-interleave-hook)

(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)

(defun pdf-view-create-page (page &optional window)
  "Create an image of PAGE for display on WINDOW."
  (let* ((size (pdf-view-desired-image-size page window))
         (width (if (not (pdf-view-use-scaling-p))
                    (car size)
                  (* 2 (car size))))
         (data (pdf-cache-renderpage
                page width width))
         (hotspots (pdf-view-apply-hotspot-functions
                    window page size)))
    (pdf-view-create-image data
      :width width
      :scale (if (pdf-view-use-scaling-p) 0.5 1)
      :map hotspots
      :pointer 'arrow)))

 (defun pdf-view-use-scaling-p ()
    "Return t if scaling should be used."
    (and (or (and (eq (framep-on-display) 'ns) (string-equal emacs-version "27.0.50"))
             (memq (pdf-view-image-type)
                   '(imagemagick image-io)))
         pdf-view-use-scaling))

;; (defun pdf-annot-show-annotation (a &optional highlight-p window)
;;     "Make annotation A visible.

;; Turn to A's page in WINDOW, and scroll it if necessary.

;; If HIGHLIGHT-P is non-nil, visually distinguish annotation A from
;; other annotations."

;;     (save-selected-window
;;       (when window (select-window window))
;;       (pdf-util-assert-pdf-window)
;;       (let* ((page (pdf-annot-get a 'page))
;;              (size (pdf-view-image-size))
;;              (width (car size))
;;         (unless (= page (pdf-view-current-page))
;;           (pdf-view-goto-page page))
;;         (let ((edges (pdf-annot-get-display-edges a)))
;;           (when highlight-p
;;             (pdf-view-display-image
;;              (pdf-view-create-image
;;                  (pdf-cache-renderpage-highlight
;;                   page width
;;                   `("white" "steel blue" 0.35 ,@edges))
;;                :map (pdf-view-apply-hotspot-functions
;;                      window page size)
;;                :width width)))
;;           (pdf-util-scroll-to-edges
;;            (pdf-util-scale-relative-to-pixel (car edges))
;;            )))))
  (defun pdf-isearch-hl-matches (current matches &optional occur-hack-p)
    "Highlighting edges CURRENT and MATCHES."
    (cl-check-type current pdf-isearch-match)
    (cl-check-type matches (list-of pdf-isearch-match))
    (cl-destructuring-bind (fg1 bg1 fg2 bg2)
        (pdf-isearch-current-colors)
      (let* ((width (car (pdf-view-image-size)))
             (page (pdf-view-current-page))
             (window (selected-window))
             (buffer (current-buffer))
             (tick (cl-incf pdf-isearch--hl-matches-tick))
             (pdf-info-asynchronous
              (lambda (status data)
                (when (and (null status)
                           (eq tick pdf-isearch--hl-matches-tick)
                           (buffer-live-p buffer)
                           (window-live-p window)
                           (eq (window-buffer window)
                               buffer))
                  (with-selected-window window
                    (when (and (derived-mode-p 'pdf-view-mode)
                               (or isearch-mode
                                   occur-hack-p)
                               (eq page (pdf-view-current-page)))
                      (pdf-view-display-image
                       (pdf-view-create-image data
                         :width width))))))))
        (pdf-info-renderpage-text-regions
         page width t nil
         `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                        current))
         `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                        (apply 'append
                               (remove current matches))))))))
  (defun pdf-util-frame-scale-factor ()
    "Return the frame scale factor depending on the image type used for display.
When `pdf-view-use-scaling' is non-nil and imagemagick or
image-io are used as the image type for display, return the
backing-scale-factor of the frame if available. If a
backing-scale-factor attribute isn't available, return 2 if the
frame's PPI is larger than 180. Otherwise, return 1."
    (if (and pdf-view-use-scaling
             (memq (pdf-view-image-type) '(imagemagick image-io))
             (fboundp 'frame-monitor-attributes))
        (or (cdr (assq 'backing-scale-factor (frame-monitor-attributes)))
            (if (>= (pdf-util-frame-ppi) 180)
                2
              1))
      (if (and (eq (framep-on-display) 'ns) (string-equal emacs-version "27.0.50"))
          2
        1)))
  (defun pdf-view-display-region (&optional region rectangle-p)
    ;; TODO: write documentation!
    (unless region
      (pdf-view-assert-active-region)
      (setq region pdf-view-active-region))
    (let ((colors (pdf-util-face-colors
                   (if rectangle-p 'pdf-view-rectangle 'pdf-view-region)
                   (bound-and-true-p pdf-view-dark-minor-mode)))
          (page (pdf-view-current-page))
          (width (car (pdf-view-image-size))))
      (pdf-view-display-image
       (pdf-view-create-image
           (if rectangle-p
               (pdf-info-renderpage-highlight
                page width nil
                `(,(car colors) ,(cdr colors) 0.35 ,@region))
             (pdf-info-renderpage-text-regions
              page width nil nil
              `(,(car colors) ,(cdr colors) ,@region)))
         :width width))))

  (defun pdf-view-create-page (page &optional window)
    "Create an image of PAGE for display on WINDOW."
    (let* ((size (pdf-view-desired-image-size page window))
           (width (car size))
           (data (pdf-cache-renderpage
                  page width (if (not (pdf-view-use-scaling-p))
                                 width
                               (* 2 width))))
           (hotspots (pdf-view-apply-hotspot-functions
                      window page size)))
      (pdf-view-create-image data
        :width width
        :map hotspots
        :pointer 'arrow)))


;; -*- lexical-binding: t -*-
(with-eval-after-load "pdf-util"
  (defun pdf-util-frame-scale-factor () 2))
(with-eval-after-load "pdf-view"
  (defun pdf-view-use-scaling-p () t))

(eval-when-compile (require 'pdf-isearch))
(with-eval-after-load "pdf-isearch"

(defun pdf-isearch-hl-matches (current matches &optional occur-hack-p)
  "Highlighting edges CURRENT and MATCHES."
  (cl-check-type current pdf-isearch-match)
  (cl-check-type matches (list-of pdf-isearch-match))
  (cl-destructuring-bind (fg1 bg1 fg2 bg2)
      (pdf-isearch-current-colors)
    (let* ((width (car (pdf-view-image-size)))
           (page (pdf-view-current-page))
           (window (selected-window))
           (buffer (current-buffer))
           (tick (cl-incf pdf-isearch--hl-matches-tick))
           (pdf-info-asynchronous
            (lambda (status data)
              (when (and (null status)
                         (eq tick pdf-isearch--hl-matches-tick)
                         (buffer-live-p buffer)
                         (window-live-p window)
                         (eq (window-buffer window)
                             buffer))
                (with-selected-window window
                  (when (and (derived-mode-p 'pdf-view-mode)
                             (or isearch-mode
                                 occur-hack-p)
                             (eq page (pdf-view-current-page)))
                    (pdf-view-display-image
                     (pdf-view-create-image data :width width)))))))) ; <-- add width
      (pdf-info-renderpage-text-regions
       page width t nil
       `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                      current))
       `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                      (apply 'append
                        (remove current matches))))))))
)

;; midnite mode hook
(add-hook 'pdf-view-mode-hook (lambda ()
                                (pdf-view-midnight-minor-mode))) ; automatically turns on midnight-mode for pdfs

(setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; set the green profile as default (see below)

(defun pdf-no-filter ()
  "View pdf without colour filter."
  (interactive)
  (pdf-view-midnight-minor-mode -1)
  )

;; change midnite mode colours functions
(defun pdf-midnite-original ()
  "Set pdf-view-midnight-colors to original colours."
  (interactive)
  (setq pdf-view-midnight-colors '("#839496" . "#002b36" )) ; original values
  (pdf-view-midnight-minor-mode)
  )

(defun pdf-midnite-amber ()
  "Set pdf-view-midnight-colors to amber on dark slate blue."
  (interactive)
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; amber
  (pdf-view-midnight-minor-mode)
  )

(defun pdf-midnite-green ()
  "Set pdf-view-midnight-colors to green on black."
  (interactive)
  (setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; green
  (pdf-view-midnight-minor-mode)
  )

(defun pdf-midnite-colour-schemes ()
  "Midnight mode colour schemes bound to keys"
  (local-set-key (kbd "!") (quote pdf-no-filter))
  (local-set-key (kbd "@") (quote pdf-midnite-amber))
  (local-set-key (kbd "#") (quote pdf-midnite-green))
  (local-set-key (kbd "$") (quote pdf-midnite-original))
  )

(add-hook 'pdf-view-mode-hook 'pdf-midnite-colour-schemes)

(lazy-unset-key
 '(".")
 pdf-view-mode-map)                     ;卸载按键
(lazy-unset-key
 '("x" "M-<" "M->")
 pdf-view-mode-map)                     ;卸载一些按键
(lazy-set-key
 '(
   ([remap scroll-up] . pdf-view-next-line-or-next-page) ;重新定向按键, 支持 auto-scroll
   )
 pdf-view-mode-map
 )
(lazy-set-key
 '(
   ("N" . pdf-view-next-page)                      ;下一页
   ("P" . pdf-view-previous-page)                  ;上一页
   ("," . pdf-view-first-page)                     ;第一页
   ("." . pdf-view-last-page)                      ;最后一页
   ("g" . pdf-view-goto-page)                      ;跳到第几页
   ("e" . pdf-view-scroll-down-or-previous-page)   ;向上滚动一屏
   ("SPC" . pdf-view-scroll-up-or-next-page)       ;向下滚动一屏
   ("j" . pdf-view-next-line-or-next-page)         ;下一行或下一屏
   ("k" . pdf-view-previous-line-or-previous-page) ;上一行或上一屏
   ("O" . pdf-occur)                               ;全局搜索
   ("q" . bury-buffer)                             ;隐藏buffer
   ("Q" . kill-this-buffer)                        ;退出
   ("s" . auto-scroll-mode)                        ;自动滚屏
   ("<" . auto-scroll-faster)                      ;加快滚屏速度
   (">" . auto-scroll-slower)                      ;减慢滚屏速度
   )
 pdf-view-mode-map
 )
;;; ### Unset key ###
;;; --- 卸载按键
(lazy-unset-key                         ;全局按键的卸载
 '("C-z" "C-q" "s-W" "s-z" "M-h" "C-x C-c"))
;;; ### Sdcv ###
;;; --- 星际译王命令行
(defvar sdcv-key-alist nil
  "The key alist that sdcv.")
(setq sdcv-key-alist
      '(("p" . sdcv-search-pointer)     ;光标处的单词, buffer显示
        ("y" . sdcv-search-pointer+)    ;光标处的单词, tooltip显示
        ("i" . sdcv-search-input)       ;输入的单词, buffer显示
        (";" . sdcv-search-input+)))    ;输入的单词, tooltip显示
(lazy-set-key sdcv-key-alist nil "C-z") ;sdcv的全局按键绑定
(lazy-set-key sdcv-key-alist pdf-view-mode-map) ;sdcv的局部按键绑定

(provide 'init-pdftools)

;;; init-pdf-tools.el ends here
