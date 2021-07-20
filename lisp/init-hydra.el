;; -*- coding: utf-8; lexical-binding: t; -*-

;; @see https://github.com/abo-abo/hydra
;; color could: red, blue, amaranth, pink, teal

;; use similar key bindings as init-evil.el
(defhydra hydra-launcher (:color blue)
  "
^Misc^                    ^Study^                    ^Audio^
--------------------------------------------------------------------------
[_ss_] Save workgroup     [_w_] Pronounce word       [_R_] Emms Random
[_ll_] Load workgroup     [_W_] Big words definition [_n_] Emms Next
[_B_] New bookmark        [_v_] Play big word video  [_p_] Emms Previous
[_m_] Goto bookmark       [_im_] Image of word       [_P_] Emms Pause
[_bb_] Switch Gnus buffer [_s1_] Pomodoro tiny task  [_O_] Emms Open
[_e_] Erase buffer        [_s2_] Pomodoro big task   [_L_] Emms Playlist
[_r_] Erase this buffer   [_st_] Pomodoro stop       [_E_] Typewriter on
[_f_] Recent file         [_sr_] Pomodoro resume     [_V_] Old typewriter
[_d_] Recent directory    [_sp_] Pomodoro pause
[_bh_] Bash history
[_hr_] Dired CMD history
[_hh_] Random theme
[_ii_] Imenu
[_q_] Quit
"
  ("hr" my-dired-redo-from-commands-history)
  ("B" bookmark-set)
  ("m" counsel-bookmark-goto)
  ("f" my-counsel-recentf)
  ("d" my-recent-directory)
  ("bh" my-insert-bash-history)
  ("hh" random-healthy-color-theme)
  ("ss" wg-create-workgroup)
  ("ii" my-counsel-imenu)
  ("ll" wg-open-workgroup)

  ("e" shellcop-erase-buffer)
  ("r" shellcop-reset-with-new-command)
  ("E" my-toggle-typewriter)
  ("V" twm/toggle-sound-style)
  ("s1" (pomodoro-start 15))
  ("s2" (pomodoro-start 60))
  ("st" pomodoro-stop)
  ("sr" pomodoro-resume)
  ("sp" pomodoro-pause)
  ("R" emms-random)
  ("n" emms-next)
  ("w" mybigword-pronounce-word)
  ("im" mybigword-show-image-of-word)
  ("W" my-lookup-big-word-definition-in-buffer)
  ("v" mybigword-play-video-of-word-at-point)
  ("p" emms-previous)
  ("P" emms-pause)
  ("O" emms-play-playlist)
  ("bb" dianyou-switch-gnus-buffer)
  ("L" emms-playlist-mode-go)
  ("q" nil :color red))

;; Because in message-mode/article-mode we've already use `y' as hotkey
(global-set-key (kbd "C-c C-y") 'hydra-launcher/body)
(defun org-mode-hook-hydra-setup ()
  (local-set-key (kbd "C-c C-y") 'hydra-launcher/body))
(add-hook 'org-mode-hook 'org-mode-hook-hydra-setup)

(with-eval-after-load 'find-file-in-project
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
    ("q" nil)))
(defun ffip-diff-mode-hook-hydra-setup ()
  (local-set-key (kbd "C-c C-y") 'hydra-ffip-diff-group/body))
(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-hydra-setup)

;; gnus-summary-mode
(with-eval-after-load 'gnus-sum
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
  (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body))

;; gnus-article-mode
(with-eval-after-load 'gnus-art
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
  (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body))

;; message-mode
(with-eval-after-load 'message
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
    ("q" nil)))

(defun message-mode-hook-hydra-setup ()
  (local-set-key (kbd "C-c C-y") 'hydra-message/body))
(add-hook 'message-mode-hook 'message-mode-hook-hydra-setup)
;; }}

;; {{ dired
;; -*- coding: utf-8; lexical-binding: t; -*-

(with-eval-after-load 'dired
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

  (defun my-extract-mkv-subtitle ()
    "Use mkvtoolnix to extract mkv subtitle."
    (interactive)
    (let* ((file (file-name-nondirectory (dired-file-name-at-point)))
           (ext (file-name-extension file))
           (default-directory (file-name-directory (dired-file-name-at-point)))
           lines
           trunks
           track-number)
      (cond
       ((not (string= "mkv" ext))
        (message "Only mkv files can be processed."))
       ((not (executable-find "mkvextract"))
        ("Please install mkvtoolnix."))
       (t
        ;; split output into trunks
        (setq trunks (split-string (shell-command-to-string (format "mkvinfo \"%s\"" file))
                                   "| ?\\+ [A-Z][^\n]+[\n]*"))
        ;; only interested english subtitle trunk
        (setq trunks (delq nil (mapcar
                                (lambda (trunk)

                                  (when (and (string-match "Track type: subtitles" trunk)
                                             (or (not (string-match "Language: " trunk))
                                                 (string-match "Language: eng" trunk)))
                                    trunk))
                                trunks)))
        (when (and (> (length trunks) 0)
                   (string-match "Track number: \\([0-9]+\\)" (car trunks)))

          ;; only extract the track number from the first truck
          (setq track-number (1- (string-to-number (match-string 1 (car trunks)))))
          (shell-command (format "mkvextract tracks \"%s\" %s:\"%s.srt\" > /dev/null 2>&1"
                                 file
                                 track-number
                                 (file-name-base file))))))))

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
^Misc^                      ^File^              ^Copy Info^
-----------------------------------------------------------------
[_vv_] video2mp3            [_R_] Move          [_pp_] Path
[_aa_] Record by mp3        [_cf_] New          [_nn_] Name
[_zz_] Play wav&mp3         [_rr_] Rename       [_bb_] Base
[_cc_] Last command         [_ff_] Find         [_dd_] directory
[_sa_] Fetch all subtitles  [_C_]  Copy
[_s1_] Fetch on subtitle    [_rb_] Change base
[_vv_] Video => Mp3         [_df_] Diff 2 files
[_aa_] Recording Wav
[_ee_] Mkv => Srt
[_+_] Create directory
"
    ("sa" (my-fetch-subtitles))
    ("s1" (my-fetch-subtitles (dired-file-name-at-point)))
    ("pp" (my-copy-file-info 'file-truename))
    ("nn" (my-copy-file-info 'file-name-nondirectory))
    ("bb" (my-copy-file-info 'file-name-base))
    ("dd" (my-copy-file-info 'file-name-directory))
    ("rb" (my-replace-dired-base (car kill-ring)))
    ("vv" my-extract-mp3-from-video)
    ("ee" my-extract-mkv-subtitle)
    ("aa" my-record-wav-by-mp3)
    ("cc" my-dired-redo-last-command)
    ("zz" my-play-both-mp3-and-wav)
    ("C" dired-do-copy)
    ("R" dired-do-rename)
    ("cf" find-file)
    ("df" my-ediff-files)
    ("rr" dired-toggle-read-only)
    ("ff" (lambda (regexp)
            (interactive "sMatching regexp: ")
            (find-lisp-find-dired default-directory regexp)))
    ("+" dired-create-directory)
    ("q" nil)))

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
(global-set-key (kbd "C-c C-t") 'hydra-toggle/body)
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
[_dd_] Diff               [_ri_] Rebase closest
[_dc_] Diff staged        [_s_] Show commit
[_dr_] Diff range         [_rr_] Reset gutter
[_au_] Add modified       [_rh_] Gutter => HEAD
[_cc_] Commit             [_l_] Log selected/file
[_ca_] Amend              [_b_] Branches
[_ja_] Amend silent       [_k_] Git commit link
[_tt_] Stash              [_Q_] Quit gutter
[_ta_] Apply stash        [_cr_] Cherry pick from reflog
[_f_] Find file in commit

"
  ("ri" my-git-rebase-interactive)
  ("rr" git-gutter-reset-to-default)
  ("rh" my-git-gutter-reset-to-head-parent)
  ("s" my-git-show-commit)
  ("l" magit-log-buffer-file)
  ("b" magit-show-refs-popup)
  ("k" git-link)
  ("g" magit-status)
  ("ta" magit-stash-apply)
  ("tt" magit-stash)
  ("dd" magit-diff-dwim)
  ("dc" magit-diff-staged)
  ("dr" (progn (magit-diff-range (my-git-commit-id))))
  ("cc" magit-commit-create)
  ("ca" magit-commit-amend)
  ("ja" (magit-commit-amend "--reuse-message=HEAD --no-verify"))
  ("au" magit-stage-modified)
  ("Q" git-gutter-toggle)
  ("f" my-git-find-file-in-commit)
  ("cr" my-git-cherry-pick-from-reflog)
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

;;multiple-cursors
(require 'multiple-cursors)
(defhydra hydra-multiple-cursors (:color blue :hint nil)
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

;;flycheck
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


;;ibuffer
(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                              :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                :after-exit
                                (if (eq major-mode 'ibuffer-mode)
                                    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)

;;lsp
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

;;org-mode-block-templates
  (defhydra hydra-org-template (:color blue :hint nil)
    "
 _c_++  _q_uote     _e_macs-lisp    _L_aTeX:
 _l_atex   _E_xample   _p_ython          _i_ndex:
 _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
 _s_rc     _n_ote      plant_u_ml      _H_TML:
 _h_tml    ^ ^         ^ ^             _A_SCII:
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("n" (hot-expand "<not"))
    ;; ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("c" (hot-expand "<s" "c++"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("p" (hot-expand "<s" "python"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
    ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (require 'org-tempo) ; Required from org 9 onwards for old template expansion
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

;;smartparent
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

(provide 'init-hydra)
;;; init-hydra.el ends here
