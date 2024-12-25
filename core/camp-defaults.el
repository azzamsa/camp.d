;;  -*- lexical-binding: t; -*-

;;; Better defaults
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)

;; === Configure advance features ===
;; I never use overwrite-mode.
(put 'overwrite-mode 'disabled t)

(put 'downcase-region 'disabled nil)


(setopt
 ;; === Default directories for builtin packages ===
 custom-theme-directory (+ensure-directory camp-etc-dir "themes/")
 auto-insert-directory (+ensure-directory camp-var-dir "auto-insert/")
 auto-save-list-file-prefix (+ensure-directory camp-var-dir "auto-save/")
 backup-directory-alist (list (cons "." (+ensure-directory camp-var-dir "backup/")))
 bookmark-default-file (concat camp-var-dir "bookmark.el")
 project-list-file (concat camp-var-dir "project-list.el")
 save-place-file (concat camp-var-dir "save-place.el")
 savehist-file (concat camp-var-dir "savehist.el")

 ;; === Additional directories for non-builtin but common packages ===
 pcache-directory (concat camp-cache-dir "pcache/")

 ;; === Default behavior ===
 ;; Inhibit startup message
 inhibit-startup-message t
 ;; Do not ring
 ring-bell-function #'ignore
 ;; Set to non-nil to flash!
 visible-bell nil
 ;; Increase the large file threshold to 50 MiB
 large-file-warning-threshold (* 50 1024 1024)
 ;; No message in scratch buffer
 initial-scratch-message nil
 ;; Set initial buffer to fundamental-mode for faster load
 initial-major-mode 'fundamental-mode
 ;; Always prompt in minibuffer (no GUI)
 use-dialog-box nil
 ;; Use y or n instead of yes or no
 use-short-answers t
 ;; Confirm before quitting
 confirm-kill-emacs #'y-or-n-p
 ;; Filter duplicate entries in kill ring
 kill-do-not-save-duplicates t
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Save files only in sub-directories of current project
 save-some-buffers-default-predicate #'save-some-buffers-root
 ;; Use single space between sentences
 sentence-end-double-space nil
 ;; Move stuff to trash
 delete-by-moving-to-trash t
 ;; Select help window for faster quit!
 help-window-select t
 ;; More info on completions
 completions-detailed t
 ;; Do not ask obvious questions, follow symlinks
 vc-follow-symlinks t
 ;; Display the true file name for symlinks
 find-file-visit-truename t
 ;; Use completion in the minibuffer instead of definitions buffer
 xref-show-definitions-function #'xref-show-definitions-completing-read
 ;; Enable recursive calls to minibuffer
 enable-recursive-minibuffers t
 ;; Kill the shell buffer after exit
 shell-kill-buffer-on-exit t
 ;; Revert non-file buffers like dired
 global-auto-revert-non-file-buffers t
 ;; Don't prompt for confirmation when we create a new file or buffer
 confirm-nonexistent-file-or-buffer nil
 ;; More intuitive buffer naming style
 uniquify-buffer-name-style 'forward

 ;; === Performances ===
 ;; Don’t compact font caches during GC
 inhibit-compacting-font-caches t
 ;; Increase single chunk bytes to read from subprocess (default 4096)
 read-process-output-max (condition-case nil
                             ;; Android may raise permission-denied error
                             (with-temp-buffer
                               (insert-file-contents "/proc/sys/fs/pipe-max-size")
                               (string-to-number (buffer-string)))
                           ;; If an error occurred, fallback to the default value
                           (error read-process-output-max))

 ;; === Aesthetics and UI ===
 ;; Do force frame size to be a multiple of char size
 frame-resize-pixelwise t
 ;; Stretch cursor to the glyph width
 x-stretch-cursor t
 ;; Resize window combinations proportionally
 window-combination-resize t
 ;; No ugly button for widgets
 widget-image-enable nil
 ;; Show unprettified symbol under cursor (when in `prettify-symbols-mode')
 prettify-symbols-unprettify-at-point t
 ;; Make tooltips last a bit longer (default 10s)
 tooltip-hide-delay 20
 ;; Use small frames to display tooltips instead of the default OS tooltips
 use-system-tooltips nil
 ;; Set line width for the divider in `window-divider-mode' to 2px
 window-divider-default-bottom-width 2
 window-divider-default-right-width 2

 ;; === Undo ===
 ;; 10MB (default is 160kB)
 undo-limit 10000000
 ;; 50MB (default is 240kB)
 undo-strong-limit 50000000
 ;; 150MB (default is 24MB)
 undo-outer-limit 150000000

 ;; === Editing ===
 ;; Hitting TAB behavior
 tab-always-indent 'complete
 ;; End files with newline
 require-final-newline t
 ;; Enable Drag-and-Drop of regions
 mouse-drag-and-drop-region t
 ;; Enable Drag-and-Drop of regions from Emacs to external programs
 mouse-drag-and-drop-region-cross-program t

 ;; === Backups ===
 ;; Disable lockfiles
 create-lockfiles nil
 ;; Enable making backup files
 make-backup-files t
 ;; Number each backup file
 version-control t
 ;; Copy instead of renaming current file
 backup-by-copying t
 ;; Clean up after itself
 delete-old-versions t
 ;; Keep up to 5 old versions of each file
 kept-old-versions 5
 ;; Keep up to 5 new versions of each file
 kept-new-versions 5

 ;; === Scrolling ===
 ;; Do not adjust window-vscroll to view tall lines. Fixes some lag issues see:
 ;; emacs.stackexchange.com/a/28746
 auto-window-vscroll nil
 ;; Fast scrolling
 fast-but-imprecise-scrolling t
 ;; Keep the point in the same position while scrolling
 scroll-preserve-screen-position t
 ;; Do not move cursor to the center when scrolling
 scroll-conservatively 101
 ;; Scroll at a margin of one line
 scroll-margin 1
 ;; The number of lines to scroll
 scroll-step 1
 ;; Columns from the window edge point allowed before horizontal scroll
 hscroll-margin 2
 ;; The number of columns to scroll
 hscroll-step 1
 ;; Make mouse scroll a little faster
 mouse-wheel-scroll-amount  '(2 ((shift) . hscroll) ((meta) . nil) ((control meta) . global-text-scale) ((control) . text-scale))
 ;; Make mouse scroll a little faster horizontally
 mouse-wheel-scroll-amount-horizontal 2

 ;; === Auto-Saving, sessions ===
 ;; Enable auto-save (use `recover-file' or `recover-session' to recover)
 auto-save-default t
 ;; Include big deletions
 auto-save-include-big-deletions t
 ;; Set file naming transform
 auto-save-file-name-transforms
 `(;; Prefix tramp autosaves with "tramp-"
   ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat auto-save-list-file-prefix "tramp-\\2") t)
   ;; Local autosaves
   (".*" ,auto-save-list-file-prefix t)))

(setq-default
 ;; === Buffer-local variables ===
 ;; Display long lines
 truncate-lines nil
 ;; Default fill column width
 fill-column 80
 ;; Never mix, use only spaces
 indent-tabs-mode nil
 ;; Width for line numbers
 display-line-numbers-width nil
 ;; Display absolute line numbers in narrowed regions
 display-line-numbers-widen t
 ;; Relative line numbering
 display-line-numbers-type 'relative
 ;; Small tab is enough!
 tab-width 2)

;; === Misc hooks and advice ===
;; Advice `emacs-session-filename' to ensure creating "session.ID" files in
;; a sub-directory
(with-eval-after-load 'x-win
  (advice-add
   #'emacs-session-filename :filter-return
   (defun +emacs-session-filename--in-subdir-a (session-filename)
     "Put the SESSION-FILENAME in the \"x-win/\" sub-directory."
     (concat (+ensure-directory camp-var-dir "x-win/")
             (file-name-nondirectory session-filename)))))

;; Kill the minibuffer when switching by mouse to another window.
;; Adapted from: trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(add-hook
 'mouse-leave-buffer-hook
 (defun +minibuffer--kill-on-mouse-h ()
   "Kill the minibuffer when switching to window with mouse."
   (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
     (abort-recursive-edit))))

;; === Tweaks on file save ===
;; Make scripts (files starting with shebang "#!") executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; === Modes enabled locally, mainly for `prog-mode', `conf-mode' and `text-mode' ===
;; Show line numbers
(add-hook 'prog-mode-hook  #'display-line-numbers-mode)
(add-hook 'conf-mode-hook  #'display-line-numbers-mode)
(add-hook 'text-mode-hook  #'display-line-numbers-mode)

;; Wrap long lines
(add-hook 'prog-mode-hook  #'visual-line-mode)
(add-hook 'conf-mode-hook  #'visual-line-mode)
(add-hook 'text-mode-hook  #'visual-line-mode)

;; === Modes enabled globally ===
;; Display divider between windows
(window-divider-mode 1)

;; Scroll pixel by pixel, in Emacs29+ there is a more pricise mode way to scroll
(if (>= emacs-major-version 29)
    (pixel-scroll-precision-mode 1)
  (pixel-scroll-mode 1))

;; Replace selection after start typing
(delete-selection-mode 1)
;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
(minibuffer-depth-indicate-mode 1)
;; Save place in files
(save-place-mode 1)
;; Enable saving minibuffer history
(savehist-mode 1)
;; Auto load files changed on disk
(global-auto-revert-mode 1)
;; Show line number in mode-line
(line-number-mode 1)
;; Show column numbers (a.k.a. cursor position) in the mode-line
(column-number-mode 1)
;; Better handling for files with so long lines
(global-so-long-mode 1)
;; Global SubWord mode
(global-subword-mode 1)

(provide 'camp-defaults)
