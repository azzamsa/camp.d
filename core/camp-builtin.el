;; -*- lexical-binding: t; -*-

(use-package transient
  :straight (:type built-in)
  :config
  ;; Map ESC and q to quit transient
  (keymap-set transient-map "<escape>" 'transient-quit-one)
  (keymap-set transient-map "q" 'transient-quit-one)

  (setq transient-history-file (expand-file-name "transient/history.el" camp-var-dir)
        transient-levels-file (expand-file-name "transient/levels.el" camp-var-dir)
        transient-values-file (expand-file-name "transient/values.el" camp-var-dir)))

(use-package password-cache
  :straight (:type built-in)
  :custom
  (password-cache t) ; Enable password caching
  (password-cache-expiry 60)) ; One minute, default is 16

(use-package auth-source
  :straight (:type built-in)
  :custom
  (auth-sources '("~/.local/share/.authinfo.gpg")) ; Default auth-sources to GPG
  (auth-source-do-cache t) ; Enable caching, do not keep asking about GPG key
  (auth-source-cache-expiry 86400)) ; All day, default is 2h (7200)

(use-package dired
  :straight (:type built-in)
  :after dirvish evil evil-collection camp-loaded
  :hook (dired-mode . dired-omit-mode)
  :init
  (+nvmap! :keymaps 'dired-mode-map
    ;; Can't rebind `dired-up-directory` to anything but `a` and `o`. Even if using `evil-collection-define-key`
    ;; to set the default dired key or the key defined by evil-collection to `nil`.
    ;; I browse mostly using one (right) hand. So `o` is more comfortable to press.
    ;; [remap dired-sort-toggle-or-edit]   '(dirvish-quicksort         :wk "Toggle or edit sort order")
    "a"          '(dired-up-directory                 :wk "Up directory")
    "o"          '(dired-up-directory                 :wk "Up directory"))
  :config
  (setopt
   ;; Keep up to 5 versions when cleaning a directory
   dired-kept-versions 5
   ;; Sort by modified time
   dired-listing-switches "-lt --almost-all --human-readable --group-directories-first --no-group --ignore=. --ignore=.."))

(use-package dired-x
  :straight (:type built-in)
  :after dired
  :config
  ;; Putting `dired-omit-files` inside `use-package dired`, `use-package dirvish`,
  ;; and `eval-after-load dired` doesn't work.
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")))

(defun +dired-copy-dirpath-as-kill ()
  "Copy the current directory path into the kill ring."
  (interactive)
  (kill-new default-directory)
  (message "Copied: %s" default-directory))

(use-package tramp
  :straight (:type built-in)
  :init
  (setq tramp-default-method "ssh")
  :config
  (setopt tramp-auto-save-directory (concat camp-var-dir "tramp/auto-save/")
          tramp-backup-directory-alist backup-directory-alist
          tramp-persistency-file-name (concat camp-var-dir "tramp/persistency.el"))
  :custom
  (tramp-default-remote-shell "/bin/bash"))

(use-package abbrev
  :straight (:type built-in)
  :init
  (setq-default abbrev-mode t)
  :config
  (setq abbrev-file-name (concat camp-etc-dir "abbrev.el"))
  (setq save-abbrevs 'silently))

;; Use built-in `treesit' when available
(use-package treesit
  :straight (:type built-in)
  :config
  ;; Tree-Sitter grammars
  (add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter" camp-var-dir))
  :custom
  (treesit-font-lock-level 4))

;; To avoid installing `tree-sitter' as this fork uses the built-in `treesit'
(push 'tree-sitter straight-built-in-pseudo-packages)

(use-package project
  :straight (:type built-in)
  :after camp-loaded
  :demand t
  :custom
  (project-list-file (concat camp-var-dir "project-list.el"))
  (project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project")))

(defun +project-from-dir (&optional dir)
  "Helper method to return project instance if DIR is a valid project."
  (project--find-in-directory dir))

(use-package tab-bar
  :straight (:type built-in)
  :custom
  ;; Do not show tabs (`tab-bar' is configured in `camp-workspaces')
  (tab-bar-show nil))

(use-package elisp-mode
  ;; defined in lisp/progmodes/elisp-mode.el
  ;; using `use-package emacs-lisp-mode' produces
  ;; so many oddities
  :straight (:type built-in)
  :after evil evil-collection camp-loaded
  :hook (prog-mode-defaults . emacs-lisp-mode)
  :config
  (evil-collection-define-key 'normal 'emacs-lisp-mode-map
    "gz" 'nil))

(use-package winner
  :straight (:type built-in)
  :config
  (winner-mode 1))

(use-package recentf
  :straight (:type built-in)
  :hook (kill-emacs . recentf-cleanup)
  :config
  (setq recentf-save-file (concat camp-var-dir "recentf-save.el")
        ;; Increase the maximum number of saved items
        recentf-max-saved-items 300
        ;; Ignore case when searching recentf files
        recentf-case-fold-search t
        ;; Exclude some files from being remembered by recentf
        recentf-exclude
        `(,(rx (* any)
               (or
                "/\\.emacs\\.d/var/"
                "eln-cache"
                "/cache/"
                ".cache/")
               (* any)
               (? (or "html" "pdf" "tex" "epub" "gz")))
          ,(rx "/"
               (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo")
               (* any))))

  (recentf-mode 1))

(use-package desktop
  :defer 2
  :config
  (setopt
   desktop-dirname (+ensure-directory camp-var-dir "desktop/")
   desktop-path (list desktop-dirname)
   ;; File name to use when saving desktop
   desktop-base-file-name "emacs-session.el"
   ;; File name to use as a lock
   desktop-base-lock-name (concat desktop-base-file-name ".lock")
   ;; Load only 5 buffers immediately, the remaining buffers will be loaded lazily
   desktop-restore-eager 5
   ;; Avoid writing contents unchanged between auto-saves
   desktop-file-checksum t
   ;; Save buffer status
   desktop-save-buffer t)

  ;; Save Emacs state from one session to another
  (desktop-save-mode 1))


(provide 'camp-builtin)
