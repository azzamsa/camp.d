;; -*- lexical-binding: t; -*-

(setq-default font-lock-multiline 'undecided)

;;; Better defaults
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq default-input-method nil)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; No hostname in frame title
;; Without setting the `icon-title-format`. The window title will revert
;; back to its original value after loosing its focus.
(setq frame-title-format '("" invocation-name " - " "%b"))
(setq icon-title-format '("" invocation-name " - " "%b"))

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Sort by modified time
(setq dired-listing-switches "-l --time=ctime --almost-all --human-readable --group-directories-first --no-group --ignore=. --ignore=..")

;; Text mode is initial mode
(setq initial-major-mode 'text-mode)

;; Text mode is default major mode
(setq default-major-mode 'text-mode)

;; Tree-Sitter grammars
(add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter" camp-var-dir))

;;; Set files and directories for built-in packages
(setq project-list-file (expand-file-name "projects" camp-var-dir)
      recentf-save-file (expand-file-name "recentf" camp-var-dir)
      auto-save-list-file-prefix (expand-file-name "autosave/" camp-var-dir)
      tramp-auto-save-directory  (expand-file-name "tramp-autosave/" camp-var-dir)
      tramp-persistency-file-name (expand-file-name "tramp-persistency" camp-var-dir)
      backup-directory-alist (list (cons "." (expand-file-name "backup/" camp-var-dir)))
      transient-history-file (expand-file-name "transient/history.el" camp-var-dir)
      transient-levels-file (expand-file-name "transient/levels.el" camp-var-dir)
      transient-values-file (expand-file-name "transient/values.el" camp-var-dir)
      eshell-aliases-file (expand-file-name "eshell/aliases" camp-var-dir)
      eshell-directory-name (expand-file-name "eshell/" camp-var-dir)
      eshell-history-file-name (expand-file-name "eshell/history" camp-var-dir)
      eshell-last-dir-ring-file-name (expand-file-name "eshell/lastdir" camp-var-dir))

(setq visible-bell nil ;; set to non-nil to flash!
      ring-bell-function 'ignore
      large-file-warning-threshold 52428800 ;; change to 50 MiB
      use-short-answers t ;; y or n istead of yes or no
      confirm-kill-emacs 'yes-or-no-p ;; confirm before quitting
      frame-resize-pixelwise t
      trash-directory nil ;; Use FreeDesktop.org trashcan (default)
      ;; Delete files to trash, as an extra layer of precaution against
      ;; accidentally deleting wanted files.
      delete-by-moving-to-trash t)

;;; Undo
(setq undo-limit        10000000 ;; 1MB (default is 160kB)
      undo-strong-limit 100000000 ;; 100MB (default is 240kB)
      undo-outer-limit  1000000000) ;; 1GB (default is 24MB)

;;; Editing
(setq-default display-line-numbers-width 3
              display-line-numbers-type 'relative
              truncate-lines nil
              fill-column 80
              tab-width 2
              indent-tabs-mode nil
              tab-always-indent nil)

;;; Backups
;; Disable backup and lockfiles
(setq create-lockfiles nil
      make-backup-files nil
      version-control t ;; number each backup file
      backup-by-copying t ;; copy instead of renaming current file
      delete-old-versions t ;; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      tramp-backup-directory-alist backup-directory-alist)

;;; Auto-Saving, sessions...
;; Enable auto-save (use `recover-file' or `recover-session' to recover)
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

(setq sentence-end-double-space nil)

;;; Scrolling
(setq hscroll-step 1
      hscroll-margin 0
      scroll-step 1
      scroll-margin 0
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)

;; Stretch cursor to the glyph width
(setq-default x-stretch-cursor t)

(setq-default window-combination-resize t)

;; Enable global modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Guess major mode when saving a file (from Doom Emacs)
(add-hook
 'after-save-hook
 (defun camp-guess-file-mode-h ()
   "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
   (when (eq major-mode 'fundamental-mode)
     (let ((buffer (or (buffer-base-buffer) (current-buffer))))
       (and (buffer-file-name buffer)
            (eq buffer (window-buffer (selected-window))) ;; Only visible buffers
            (set-auto-mode))))))

(with-eval-after-load 'camp-loaded
  ;; Highlight current line
  (global-hl-line-mode 1)
  ;; Enable recentf-mode globally
  (recentf-mode 1)
  ;; Global SubWord mode
  (global-subword-mode 1))

(provide 'camp-defaults)
