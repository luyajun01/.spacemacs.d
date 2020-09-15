;; -*- coding: utf-8; lexical-binding: t; -*-

;; @see https://github.com/abo-abo/hydra
;; color could: red, blue, amaranth, pink, teal
;;hydra
;;;yank
  (defhydra hydra-yank-pop ()
    "yank"
    ("C-y" yank nil)
    ("M-y" yank-pop nil)
    ("y" (yank-pop 1) "next")
    ("Y" (yank-pop -1) "prev")
    ("l" helm-show-kill-ring "list" :color blue)) ; or browse-kill-ring
  (global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
  (global-set-key (kbd "C-y") #'hydra-yank-pop/yank)

  ;;movement
  (global-set-key
   (kbd "C-n")
   (defhydra hydra-move
     (:body-pre (next-line))
     "move"
     ("n" next-line)
     ("p" previous-line)
     ("f" forward-char)
     ("b" backward-char)
     ("a" beginning-of-line)
     ("e" move-end-of-line)
     ("v" scroll-up-command)
     ;; Converting M-v to V here by analogy.
     ("V" scroll-down-command)
     ("l" recenter-top-bottom)))

  ;;page navigation
  (defhydra hydra-page (ctl-x-map "" :pre (widen))
    "page"
    ("]" forward-page "next")
    ("[" backward-page "prev")
    ("n" narrow-to-page "narow" :bind nil :exit t))
  ;;goto line
  (defhydra hydra-goto-line (goto-map ""
                                      :pre (linum-mode 1)
                                      :post (linum-mode -1))
    "goto-line"
    ("g" goto-line "go")
    ("m" set-mark-command "mark" :bind nil)
    ("q" nil "quit"))
  (global-set-key (kbd "M-g C-m") #'hydra-goto-line/set-mark-command)
  ;;Move Text
  (defhydra hydra-move-text ()
    "Move text"
    ("u" move-text-up "up")
    ("d" move-text-down "down"))

  ;;find file with xf
  (defun x-hydra-pre ()
    (insert "x")
    (let ((timer (timer-create)))
      (timer-set-time timer (timer-relative-time (current-time) 0.5))
      (timer-set-function timer 'hydra-keyboard-quit)
      (timer-activate timer)))

  (defhydra x-hydra (:body-pre x-hydra-pre
                               :color blue
                               :hint nil)
    ("f" (progn (zap-to-char -1 ?x) (ido-find-file))))

  (global-set-key "x" #'x-hydra/body)

  ;;outline minor mode
  (defhydra hydra-outline (:color pink :hint nil)
    "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
    ;; Hide
    ("q" hide-sublevels)      ; Hide everything but the top-level headings
    ("t" hide-body)           ; Hide everything but headings (all body lines)
    ("o" hide-other)          ; Hide other branches
    ("c" hide-entry)          ; Hide this entry's body
    ("l" hide-leaves)         ; Hide body lines in this entry and sub-entries
    ("d" hide-subtree)        ; Hide everything in this entry and sub-entries
    ;; Show
    ("a" show-all)            ; Show (expand) everything
    ("e" show-entry)          ; Show this heading's body
    ("i" show-children)       ; Show this heading's immediate child sub-headings
    ("k" show-branches)       ; Show all sub-headings under this heading
    ("s" show-subtree)        ; Show (expand) everything in this heading & below
    ;; Move
    ("u" outline-up-heading)               ; Up
    ("n" outline-next-visible-heading)     ; Next
    ("p" outline-previous-visible-heading) ; Previous
    ("f" outline-forward-same-level)       ; Forward - same level
    ("b" outline-backward-same-level)      ; Backward - same level
    ("z" nil "leave"))
  (global-set-key (kbd "C-c #") 'hydra-outline/body) ; by example
                                        ;goto
  (defhydra goto (:color blue :hint nil)
    "
Goto:
^Char^              ^Word^                ^org^                    ^search^
^^^^^^^^---------------------------------------------------------------------------
_c_: 2 chars        _w_: word by char     _h_: headline in buffer  _o_: helm-occur
_C_: char           _W_: some word        _a_: heading in agenda   _p_: helm-swiper
_L_: char in line   _s_: subword by char  _q_: swoop org buffers   _f_: search forward
^  ^                _S_: some subword     ^ ^                      _b_: search backward
-----------------------------------------------------------------------------------
_B_: helm-buffers       _l_: avy-goto-line
_m_: helm-mini          _i_: ace-window
_R_: helm-recentf

_n_: Navigate           _._: mark position _/_: jump to mark
"
    ("c" avy-goto-char-2)
    ("C" avy-goto-char)
    ("L" avy-goto-char-in-line)
    ("w" avy-goto-word-1)
    ;; jump to beginning of some word
    ("W" avy-goto-word-0)
    ;; jump to subword starting with a char
    ("s" avy-goto-subword-1)
    ;; jump to some subword
    ("S" avy-goto-subword-0)
    ("l" avy-goto-line)
    ("i" ace-window)
    ("h" helm-org-headlines)
    ("a" helm-org-agenda-files-headings)
    ("q" helm-multi-swoop-org)

    ("o" helm-occur)
    ("p" swiper-helm)

    ("f" isearch-forward)
    ("b" isearch-backward)

    ("." org-mark-ring-push :color red)
    ("/" org-mark-ring-goto :color blue)
    ("B" helm-buffers-list)
    ("m" helm-mini)
    ("R" helm-recentf)
    ("n" hydra-navigate/body))

  (global-set-key (kbd "s-g") 'goto/body)
  ;;hydra skan-user-buffer
  (defun my/name-of-buffers (n)
    "Return the names of the first N buffers from `buffer-list'."
    (let ((bns
           (delq nil
                 (mapcar
                  (lambda (b)
                    (unless (string-match "^ " (setq b (buffer-name b)))
                      b))
                  (buffer-list)))))
      (subseq bns 1 (min (1+ n) (length bns)))))

  ;; Given ("a", "b", "c"), return "1. a, 2. b, 3. c".
  (defun my/number-names (list)
    "Enumerate and concatenate LIST."
    (let ((i 0))
      (mapconcat
       (lambda (x)
         (format "%d. %s" (cl-incf i) x))
       list
       ", ")))

  (defvar my/last-buffers nil)

  (defun my/switch-to-buffer (arg)
    (interactive "p")
    (switch-to-buffer
     (nth (1- arg) my/last-buffers)))

  (defun my/switch-to-buffer-other-window (arg)
    (interactive "p")
    (switch-to-buffer-other-window
     (nth (1- arg) my/last-buffers)))
  (global-set-key
   "\C-o"
   (defhydra my/switch-to-buffer (:exit t
                                        :body-pre (setq my/last-buffers
                                                        (my/name-of-buffers 4)))
     "
other buffers: %s(my/number-names my/last-buffers)

"
     ("o" (my/switch-to-buffer 1))
     ("1" (my/switch-to-buffer 1))
     ("2" (my/switch-to-buffer 2))
     ("3" (my/switch-to-buffer 3))
     ("4" (my/switch-to-buffer 4))
     ("<escape>" nil)))
  ;; hydra-avy
  (defhydra hydra-avy (:exit t :hint nil)
    "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
    ("c" avy-goto-char-timer)
    ("C" avy-goto-char)
    ("w" avy-goto-word-1)
    ("W" avy-goto-word-0)
    ("l" avy-goto-line)
    ("L" avy-goto-end-of-line)
    ("m" avy-move-line)
    ("M" avy-move-region)
    ("k" avy-kill-whole-line)
    ("K" avy-kill-region)
    ("y" avy-copy-line)
    ("Y" avy-copy-region))
  ;; hydra-smartparens
  (defhydra hydra-smartparens (:hint nil)
    "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
    ;; Moving
    ("a" sp-beginning-of-sexp)
    ("e" sp-end-of-sexp)
    ("f" sp-forward-sexp)
    ("b" sp-backward-sexp)
    ("n" sp-down-sexp)
    ("N" sp-backward-down-sexp)
    ("p" sp-up-sexp)
    ("P" sp-backward-up-sexp)
  
    ;; Slurping & barfing
    ("h" sp-backward-slurp-sexp)
    ("H" sp-backward-barf-sexp)
    ("l" sp-forward-slurp-sexp)
    ("L" sp-forward-barf-sexp)
  
    ;; Wrapping
    ("R" sp-rewrap-sexp)
    ("u" sp-unwrap-sexp)
    ("U" sp-backward-unwrap-sexp)
    ("(" sp-wrap-round)
    ("{" sp-wrap-curly)
    ("[" sp-wrap-square)
    ;; Sexp juggling
    ("S" sp-split-sexp)
    ("s" sp-splice-sexp)
    ("r" sp-raise-sexp)
    ("j" sp-join-sexp)
    ("t" sp-transpose-sexp)
    ("A" sp-absorb-sexp)
    ("E" sp-emit-sexp)
    ("o" sp-convolute-sexp)
    ;; Destructive editing
    ("c" sp-change-inner :exit t)
    ("C" sp-change-enclosing :exit t)
    ("k" sp-kill-sexp)
    ("K" sp-backward-kill-sexp)
    ("w" sp-copy-sexp)
    ("q" nil)
    ("g" nil))
  ;; hydra-org-template
  (defhydra hydra-org-template (:color blue :hint nil)
    "
 _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
 _l_atex   _E_xample   _p_erl          _i_ndex:
 _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
 _s_rc     _n_ote      plant_u_ml      _H_TML:
 _h_tml    _r_         ^ ^             _A_SCII:
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("n" (hot-expand "<not"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("p" (hot-expand "<s" "python"))
    ("j" (hot-expand "<s" "java"))
    ("c" (hot-expand "<s" "c :includes <stdio.h>"))
    ("C" (hot-expand "<s" "c++ :includes <Rcpp.h>"))
    ;; ("p" (hot-expand "<s" "perl"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
    ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
    ("r" (hot-expand "<s" "R :results output graphics :file fig_1.png :exports both "))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))
  (require 'org-tempo)  ; Required from org 9 onwards for old template expansion
  ;; Reset the org-template expnsion system, this is need after upgrading to org 9 for some reason
  (setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))
  (defun hot-expand (str &optional mod header)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end))
        (deactivate-mark))
      (when header (insert "#+HEADER: " header) (forward-line))
      (insert str)
      (org-tempo-complete-tag)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
          (hydra-org-template/body)
        (self-insert-command 1))))

  (eval-after-load "org"
    '(cl-pushnew
      '("not" . "note")
      org-structure-template-alist))
  ;; hydra-yasnippet
  (defhydra hydra-yasnippet (:color blue :hint nil)
    "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
    ("d" yas-load-directory)
    ("e" yas-activate-extra-mode)
    ("i" yas-insert-snippet)
    ("f" yas-visit-snippet-file :color blue)
    ("n" yas-new-snippet)
    ("t" yas-tryout-snippet)
    ("l" yas-describe-tables)
    ("g" yas/global-mode)
    ("m" yas/minor-mode)
    ("a" yas-reload-all))
  ;; hydra-org
  (defhydra hydra-org (:color blue :timeout 12 :columns 4)
    "Org commands"
    ;; ("i" (lambda () (interactive) (org-clock-in '(4))) "Clock in")
    ;; ("o" org-clock-out "Clock out")
    ;; ("q" org-clock-cancel "Cancel a clock")
    ;; ("<f10>" org-clock-in-last "Clock in the last task")
    ;; ("j" (lambda () (interactive) (org-clock-goto '(4))) "Go to a clock")
    ;; ("m" make-this-message-into-an-org-todo-item "Flag and capture this message")
    ("l" org-toggle-latex-fragment "latex-pic")
    ("n" org-next-visible-heading "next-heading")
    ("p" org-previous-visible-heading "previous-heading")
    ("b" spacemacs/org-bold "font-bold")
    )
  ;; hydra-lsp
  (defhydra hydra-lsp (:exit t :hint nil)
    "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
    ("d" lsp-find-declaration)
    ("D" lsp-ui-peek-find-definitions)
    ("R" lsp-ui-peek-find-references)
    ("i" lsp-ui-peek-find-implementation)
    ("t" lsp-find-type-definition)
    ("s" lsp-signature-help)
    ("o" lsp-describe-thing-at-point)
    ("r" lsp-rename)
    ("f" lsp-format-buffer)
    ("m" lsp-ui-imenu)
    ("x" lsp-execute-code-action)
    ("M-s" lsp-describe-session)
    ("M-r" lsp-restart-workspace)
    ("S" lsp-shutdown-workspace))
;;; hydra-vi
  (defhydra hydra-vi ()
    "vi"
    ("h" backward-char "backward-char")
    ("j" next-line "next-line")
    ("k" previous-line "previous-line")
    ("l" forward-char "forward-char")
    ("." hydra-repeat "hydra-repeat"))
  ;; hydra-zoom
  (defun hydra-zoom/body ()
    "Call the body in the \"hydra-zoom\" hydra.

The heads for the associated hydra are:

\"g\":    `text-scale-increase',
\"l\":    `text-scale-decrease'

The body can be accessed via `hydra-zoom/body', which is bound to \"<f2>\"."
    (interactive)
    (require 'hydra)
    (hydra-default-pre)
    (let ((hydra--ignore nil))
      (hydra-keyboard-quit)
      (setq hydra-curr-body-fn
            'hydra-zoom/body))
    (hydra-show-hint
     hydra-zoom/hint
     'hydra-zoom)
    (hydra-set-transient-map
     hydra-zoom/keymap
     (lambda nil
       (hydra-keyboard-quit)
       nil)
     nil)
    (setq prefix-arg
          current-prefix-arg))
  ;; hydra-toggle
  (defhydra hydra-toggle (:exit t)
    ("a" abbrev-mode "abbrev")
    ("d" toggle-debug-on-error "debug")
    ("f" auto-fill-mode "fill")
    ("t" toggle-truncate-lines "truncate")
    ("w" whitespace-mode "whitespace")
    ("q" nil "quit"))
  ;; hydra-window
  (defhydra hydra-window
    (:color red :hint nil)
    "
                               -- WINDOW MENU --

"
    ("z" ace-window "ace" :color blue :column "1-Switch")
    ("h" windmove-left "← window")
    ("j" windmove-down "↓ window")
    ("k" windmove-up "↑ window")
    ("l" windmove-right "→ window")
    ("s" split-window-below "split window" :color blue :column "2-Split Management")
    ("v" split-window-right "split window vertically" :color blue)
    ("d" delete-window "delete current window")
    ("f" follow-mode "toogle follow mode")
    ("u" winner-undo "undo window conf" :column "3-Undo/Redo")
    ("r" winner-redo "redo window conf")
    ("b" balance-windows "balance window height" :column "4-Sizing")
    ("m" maximize-window "maximize current window")
    ("M" minimize-window "minimize current window")
    ("q" nil "quit menu" :color blue :column nil))
  ;; hydra-flycheck
  (defhydra hydra-flycheck
    (:pre (flycheck-list-errors)
          :post (quit-windows-on "*Flycheck errors*")
          :hint nil)
    "Errors"
    ("f" flycheck-error-list-set-filter "Filter")
    ("j" flycheck-next-error "Next")
    ("k" flycheck-previous-error "Previous")
    ("gg" flycheck-first-error "First")
    ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q" nil))
;;; hydra-dired
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff) ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy) ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer) ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay) ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file) ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
  ;; hydra-projectile
  (defhydra hydra-projectile-other-window (:color teal)
    "projectile-other-window"
    ("f"  projectile-find-file-other-window        "file")
    ("g"  projectile-find-file-dwim-other-window   "file dwim")
   ("d"  projectile-find-dir-other-window         "dir")
    ("b"  projectile-switch-to-buffer-other-window "buffer")
    ("q"  nil                                      "cancel" :color blue))

  (defhydra hydra-projectile (:color teal
                                     :hint nil)
    "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
    ("a"   projectile-ag)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("s-f" projectile-find-file)
    ("ff"  projectile-find-file-dwim)
    ("fd"  projectile-find-file-in-directory)
    ("g"   ggtags-update-tags)
    ("s-g" ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("s-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("s-p" projectile-switch-project "switch project")
    ("p"   projectile-switch-project)
    ("s"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("`"   hydra-projectile-other-window/body "other window")
    ("q"   nil "cancel" :color blue))


;;ace-window
;; (global-set-key (kbd "M-o") 'ace-window)
;; (defvar aw-dispatch-alist
;;   '((?x aw-delete-window "Delete Window")
;; 	(?m aw-swap-window "Swap Windows")
;; 	(?M aw-move-window "Move Window")
;; 	(?c aw-copy-window "Copy Window")
;; 	(?j aw-switch-buffer-in-window "Select Buffer")
;; 	(?n aw-flip-window)
;; 	(?u aw-switch-buffer-other-window "Switch Buffer Other Window")
;; 	(?c aw-split-window-fair "Split Fair Window")
;; 	(?v aw-split-window-vert "Split Vert Window")
;; 	(?b aw-split-window-horz "Split Horz Window")
;; 	(?o delete-other-windows "Delete Other Windows")
;; 	(?? aw-show-dispatch-help))
;;   "List of actions for `aw-dispatch-default'.")
;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; mc/num-cursors is not autoloaded
(require 'multiple-cursors)

(defhydra hydra-multiple-cursors (:hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))
;; use similar key bindings as init-evil.el
(defhydra hydra-launcher (:color blue)
  "
^Misc^                    ^Audio^               ^Move^                          ^Pomodoro^
----------------------------------------------------------------------------------------------
[_ss_] Save workgroup     [_R_] Emms Random     [_sa_] Backward Sentence (M-a)  [_ss_] Start
[_ll_] Load workgroup     [_n_] Emms Next       [_se_] Forward Sentence (M-e)   [_st_] Stop
[_B_] New bookmark        [_p_] Emms Previous   [_la_] Backward Up List         [_sr_] Resume
[_m_] Goto bookmark       [_P_] Emms Pause      [_le_] Forward List             [_sp_] Pause
[_v_] Show/Hide undo      [_O_] Emms Open       [_pa_] Backward Paragraph (M-{)
[_b_] Switch Gnus buffer  [_L_] Emms Playlist   [_pe_] Forward Paragraph (M-})
[_f_] Recent file         [_w_] Pronounce word
[_d_] Recent directory
[_h_] Dired CMD history
[_E_] Enable typewriter
[_V_] Vintage typewriter
[_q_] Quit
"
  ("h" my-dired-redo-from-commands-history)
  ("B" bookmark-set)
  ("m" counsel-bookmark-goto)
  ("f" my-counsel-recentf)
  ("d" counsel-recent-directory)
  ("ss" wg-create-workgroup)
  ("ll" my-wg-switch-workgroup)
  ("E" toggle-typewriter)
  ("V" twm/toggle-sound-style)
  ("v" undo-tree-visualize)
  ("ss" pomodoro-start)
  ("st" pomodoro-stop)
  ("sr" pomodoro-resume)
  ("sp" pomodoro-pause)
  ("sa" backward-sentence)
  ("se" forward-sentence)
  ("la" backward-up-list)
  ("le" forward-list)
  ("pa" backward-paragraph)
  ("pe" forward-paragraph)
  ("R" emms-random)
  ("n" emms-next)
  ("w" my-pronounce-current-word)
  ("p" emms-previous)
  ("P" emms-pause)
  ("O" emms-play-playlist)
  ("b" dianyou-switch-gnus-buffer)
  ("L" emms-playlist-mode-go)
  ("q" nil :color red))

;; Because in message-mode/article-mode we've already use `y' as hotkey
(global-set-key (kbd "C-c C-y") 'hydra-launcher/body)
(defun org-mode-hook-hydra-setup ()
  (local-set-key (kbd "C-c C-y") 'hydra-launcher/body))
(add-hook 'org-mode-hook 'org-mode-hook-hydra-setup)

(eval-after-load 'find-file-in-project
  '(progn
     (defhydra hydra-ffip-diff-group (:color blue)
       "
[_k_] Previous hunk
[_j_] Next hunk
[_p_] Previous file
[_n_] Next file
"
       ("k" diff-hunk-prev)
       ("j" diff-hunk-next)
       ("p" diff-file-prev)
       ("n" diff-file-next)
       ("q" nil))))
(defun ffip-diff-mode-hook-hydra-setup ()
  (local-set-key (kbd "C-c C-y") 'hydra-ffip-diff-group/body))
(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-hydra-setup)

;; gnus-summary-mode
(eval-after-load 'gnus-sum
  '(progn
     (defhydra hydra-gnus-summary (:color blue)
       "
[_F_] Forward (C-c C-f)             [_s_] Show thread
[_e_] Resend (S D e)                [_h_] Hide thread
[_r_] Reply                         [_n_] Refresh (/ N)
[_R_] Reply with original           [_!_] Mail -> disk
[_w_] Reply all (S w)               [_d_] Disk -> mail
[_W_] Reply all with original (S W) [_c_] Read all
[_G_] Search current folder         [_#_] Mark
[_b_] Switch Gnus buffer            [_A_] Show Raw article
"
       ("s" gnus-summary-show-thread)
       ("h" gnus-summary-hide-thread)
       ("n" gnus-summary-insert-new-articles)
       ("F" gnus-summary-mail-forward)
       ("!" gnus-summary-tick-article-forward)
       ("b" dianyou-switch-gnus-buffer)
       ("d" gnus-summary-put-mark-as-read-next)
       ("c" gnus-summary-catchup-and-exit)
       ("e" gnus-summary-resend-message-edit)
       ("R" gnus-summary-reply-with-original)
       ("r" gnus-summary-reply)
       ("W" gnus-summary-wide-reply-with-original)
       ("w" gnus-summary-wide-reply)
       ("#" gnus-topic-mark-topic)
       ("A" gnus-summary-show-raw-article)
       ("G" dianyou-group-make-nnir-group)
       ("q" nil))
     ;; y is not used by default
     (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue)
       "
[_o_] Save attachment        [_F_] Forward
[_v_] Play video/audio       [_r_] Reply
[_d_] CLI to download stream [_R_] Reply with original
[_b_] Open external browser  [_w_] Reply all (S w)
[_f_] Click link/button      [_W_] Reply all with original (S W)
[_g_] Focus link/button      [_b_] Switch Gnus buffer
"
       ("F" gnus-summary-mail-forward)
       ("r" gnus-article-reply)
       ("R" gnus-article-reply-with-original)
       ("w" gnus-article-wide-reply)
       ("W" gnus-article-wide-reply-with-original)
       ("o" (lambda () (interactive) (let* ((file (gnus-mime-save-part))) (when file (copy-yank-str file)))))
       ("v" w3mext-open-with-mplayer)
       ("d" w3mext-download-rss-stream)
       ("b" w3mext-open-link-or-image-or-url)
       ("f" w3m-lnum-follow)
       ("g" w3m-lnum-goto)
       ("b" dianyou-switch-gnus-buffer)
       ("q" nil))
     ;; y is not used by default
     (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

;; message-mode
(eval-after-load 'message
  '(progn
     (defhydra hydra-message (:color blue)
  "
[_c_] Complete mail address [_H_] convert to html mail
[_a_] Attach file           [_p_] Paste image from clipboard
[_s_] Send mail (C-c C-c)
[_b_] Switch Gnus buffer
[_i_] Insert email address
"
       ("c" counsel-bbdb-complete-mail)
       ("a" mml-attach-file)
       ("s" message-send-and-exit)
       ("b" dianyou-switch-gnus-buffer)
       ("i" dianyou-insert-email-address-from-received-mails)
       ("H" org-mime-htmlize)
       ("p" dianyou-paste-image-from-clipboard)
       ("q" nil))))

(defun message-mode-hook-hydra-setup ()
  (local-set-key (kbd "C-c C-y") 'hydra-message/body))
(add-hook 'message-mode-hook 'message-mode-hook-hydra-setup)
;; }}

;; {{ dired
(eval-after-load 'dired
  '(progn
     (defun my-replace-dired-base (base)
       "Change file name in `wdired-mode'"
       (let* ((fp (dired-file-name-at-point))
              (fb (file-name-nondirectory fp))
              (ext (file-name-extension fp))
              (dir (file-name-directory fp))
              (nf (concat base "." ext)))
         (when (yes-or-no-p (format "%s => %s at %s?"
                                    fb nf dir))
           (rename-file fp (concat dir nf)))))
     (defun my-extract-mp3-from-video ()
       "Extract mp3 from current video file using ffmpeg."
       (interactive)
       (let* ((video-file (file-name-nondirectory (dired-file-name-at-point)))
              (params (split-string (string-trim (read-string "Please input start-second [total seconds] (e.g, \"6 10\" or \"05:30 5\") or just press enter: "))
                                    " +"))
              (start (car params))
              (total (if (eq (length params) 1) "5" (nth 1 params)))
              cmd)
         (cond
          ((string= start "")
           ;; extract audio to MP3 with sample rate 44.1Khz (CD quality), stereo, and 2 channels
           (setq cmd (format "ffmpeg -i \"%s\" -vn -ar 44100 -ac 2 -ab 192 -f mp3 \"%s\""
                             video-file
                             (concat (file-name-base video-file) ".mp3"))))
          (t
           (setq cmd (format "ffmpeg -i \"%s\" -vn -ss %s -t %s -acodec copy \"%s\""
                             video-file
                             start
                             total
                             (format "%s-%s-%s.mp3" (file-name-base video-file) start total)))))
           (shell-command (concat cmd " &"))))

     (defun my-record-wav-by-mp3 ()
       "Record a wav using meta data from current mp3 file."
       (interactive)
       (let* ((mp3-file (file-name-nondirectory (dired-file-name-at-point)))
              (base (file-name-base mp3-file))
              (params (split-string base  "-"))
              (output-file (concat base ".wav"))
              (total (string-to-number (nth (1- (length params)) params)))
              cmd)
         (if (= total 0) (setq total 4))
         (setq cmd (format "arecord -fdat -d %s \"%s\""
                           total
                           output-file))
           (message "Start recording %s seconds wav ..." total)
           (my-async-shell-command cmd)))
     (defun my-play-both-mp3-and-wav ()
       "Play wav and mp3."
       (interactive)
       (let* ((audio-file (file-name-nondirectory (dired-file-name-at-point)))
              (base (file-name-base audio-file))
              (ext (file-name-extension audio-file) )
              (cmd (format "mplayer -quiet \"%s\" \"%s\""
                           audio-file
                           (concat base "." (if (string= ext "mp3") "wav" "mp3")))))
         (my-async-shell-command cmd)))
     (defun my-copy-file-info (fn)
       (message "%s => clipboard & yank ring"
                (copy-yank-str (funcall fn (dired-file-name-at-point)))))
     (defhydra hydra-dired (:color blue)
       "
^Misc^                      ^File^             ^Copy Info^
----------------------------------------------------------------
[_vv_] video2mp3            [_R_] Move         [_pp_] Path
[_aa_] Record by mp3        [_cf_] New         [_nn_] Name
[_zz_] Play wav&mp3         [_rr_] Rename      [_bb_] Base
[_cc_] Last command         [_ff_] Find        [_dd_] directory
[_sa_] Fetch all subtitles  [_C_]  Copy
[_s1_] Fetch on subtitle    [_rb_] Change base
[_+_] Create directory
"
       ("sa" (shell-command "periscope.py -l en *.mkv *.mp4 *.avi &"))
       ("s1" (let* ((video-file (dired-file-name-at-point))
                    (default-directory (file-name-directory video-file)))
               (shell-command (format "periscope.py -l en %s &" (file-name-nondirectory video-file)))))
       ("pp" (my-copy-file-info 'file-truename))
       ("nn" (my-copy-file-info 'file-name-nondirectory))
       ("bb" (my-copy-file-info 'file-name-base))
       ("dd" (my-copy-file-info 'file-name-directory))
       ("rb" (my-replace-dired-base (car kill-ring)))
       ("vv" my-extract-mp3-from-video)
       ("aa" my-record-wav-by-mp3)
       ("cc" my-dired-redo-last-command)
       ("zz" my-play-both-mp3-and-wav)
       ("C" dired-do-copy)
       ("R" dired-rename-file)
       ("cf"find-file)
       ("rr" dired-toggle-read-only)
       ("ff" (lambda (regexp)
               (interactive "sMatching regexp: ")
               (find-lisp-find-dired default-directory regexp)))
       ("+" dired-create-directory)
       ("q" nil))))

(defun dired-mode-hook-hydra-setup ()
  (local-set-key (kbd "y") 'hydra-dired/body))
(add-hook 'dired-mode-hook 'dired-mode-hook-hydra-setup)
;; }}

;; increase and decrease font size in GUI emacs
;; @see https://oremacs.com/download/london.pdf
(when (display-graphic-p)
  ;; Since we already use GUI Emacs, f2 is definitely available
  (defhydra hydra-zoom (global-map "<f2>")
    "Zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")
    ("r" (text-scale-set 0) "reset")
    ("0" (text-scale-set 0) :bind nil :exit t)
    ("1" (text-scale-set 0) nil :bind nil :exit t)))
(defvar whitespace-mode nil)

;; {{ @see https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defhydra hydra-toggle (:color pink)
  "
_u_ company-ispell     %(if (memq 'company-ispell company-backends) t)
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode
_i_ indent-tabs-mode:   %`indent-tabs-mode
"
  ("u" toggle-company-ispell nil)
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("i" (lambda () (interactive) (setq indent-tabs-mode (not indent-tabs-mode))) nil)
  ("q" nil "quit"))
;; Recommended binding:
(global-set-key (kbd "C-c C-h") 'hydra-toggle/body)
;; }}

;; {{ @see https://github.com/abo-abo/hydra/wiki/Window-Management

;; helpers from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-window ()
  "
Movement^^   ^Split^         ^Switch^     ^Resize^
-----------------------------------------------------
_h_ Left     _v_ertical      _b_uffer     _q_ X left
_j_ Down     _x_ horizontal  _f_ind files _w_ X Down
_k_ Top      _z_ undo        _a_ce 1      _e_ X Top
_l_ Right    _Z_ reset       _s_wap       _r_ X Right
_F_ollow     _D_elete Other  _S_ave       max_i_mize
_SPC_ cancel _o_nly this     _d_elete
"
  ("h" windmove-left )
  ("j" windmove-down )
  ("k" windmove-up )
  ("l" windmove-right )
  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)
  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("o" delete-other-windows)
  ("i" ace-delete-other-windows)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("Z" winner-redo)
  ("SPC" nil))
(global-set-key (kbd "C-c C-w") 'hydra-window/body)
;; }}

;; {{ git-gutter, @see https://github.com/abo-abo/hydra/wiki/Git-gutter
(defhydra hydra-git (:body-pre
                     (progn
                       (git-gutter-mode 1)
                       (setq git-link-use-commit t))
                     :after-exit (setq git-link-use-commit nil)
                     :color blue)
"
Git:
[_i_] Gist selected      [_dd_] Diff
[_s_] Show commit        [_dc_] Diff staged
[_r_] Reset gutter       [_dr_] Diff range
[_h_] Gutter => HEAD     [_au_] Add modified
[_l_] Log selected/file  [_cc_] Commit
[_b_] Branches           [_ca_] Amend
[_k_] Git commit link    [_tt_] Stash
[_Q_] Quit gutter        [_ta_] Apply Stash
"
  ("i" gist-region)
  ("r" git-gutter-reset-to-default)
  ("s" my-git-show-commit)
  ("l" magit-log-buffer-file)
  ("b" magit-show-refs-popup)
  ("h" git-gutter-reset-to-head-parent)
  ("k" git-link)
  ("g" magit-status)
  ("ta" magit-stash-apply)
  ("tt" magit-stash)
  ("dd" magit-diff-dwim)
  ("dc" magit-diff-staged)
  ("dr" (progn (magit-diff-range (my-git-commit-id))))
  ("cc" magit-commit-popup)
  ("ca" magit-commit-amend)
  ("au" magit-stage-modified)
  ("Q" git-gutter-toggle)
  ("q" nil))
(global-set-key (kbd "C-c C-g") 'hydra-git/body)
;; }}

(defhydra hydra-search ()
  "
 ^Search^         ^Dictionary^
-----------------------------------------
_g_ Google        _b_ English => English
_f_ Finance       _t_ English => Chinese
_s_ StackOverflow _d_ dict.org
_h_ Code
_m_ Man
"
  ("b" sdcv-search-input)
  ("t" sdcv-search-input+)
  ("d" my-lookup-dict-org)
  ("g" w3m-google-search)
  ("f" w3m-search-financial-dictionary)
  ("s" w3m-stackoverflow-search)
  ("h" w3mext-hacker-search)
  ("m" lookup-doc-in-man)
  ("q" nil))
(global-set-key (kbd "C-c C-s") 'hydra-search/body)

(defhydra hydra-describe (:color blue :hint nil)
  "
Describe Something: (q to quit)
_a_ all help for everything screen
_b_ bindings
_B_ personal bindings
_c_ char
_C_ coding system
_f_ function
_F_ flycheck checker
_i_ input method
_k_ key briefly
_K_ key
_l_ language environment
_L_ mode lineage
_m_ major mode
_M_ minor mode
_n_ current coding system briefly
_N_ current coding system full
_o_ lighter indicator
_O_ lighter symbol
_p_ package
_P_ text properties
_s_ symbol
_t_ theme
_v_ variable
_w_ where is something defined
"
  ("b" describe-bindings)
  ("B" describe-personal-keybindings)
  ("C" describe-categories)
  ("c" describe-char)
  ("C" describe-coding-system)
  ("f" describe-function)
  ("F" flycheck-describe-checker)
  ("i" describe-input-method)
  ("K" describe-key)
  ("k" describe-key-briefly)
  ("l" describe-language-environment)
  ("L" help/parent-mode-display)
  ("M" describe-minor-mode)
  ("m" describe-mode)
  ("N" describe-current-coding-system)
  ("n" describe-current-coding-system-briefly)
  ("o" describe-minor-mode-from-indicator)
  ("O" describe-minor-mode-from-symbol)
  ("p" describe-package)
  ("P" describe-text-properties)
  ("q" nil)
  ("a" help)
  ("s" describe-symbol)
  ("t" describe-theme)
  ("v" describe-variable)
  ("w" where-is))
(global-set-key (kbd "C-c C-q") 'hydra-describe/body)

(provide 'init-hydra)
;;; init-hydra.el ends here
