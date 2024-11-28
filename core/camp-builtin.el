;; -*- lexical-binding: t; -*-

(use-package transient
  :straight t
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
  :hook (dired-mode . dired-omit-mode)
  :after camp-loaded
  :config
  (evil-set-initial-state 'dired 'emacs)

  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask))

(defun ora-dired-up-directory ()
  (interactive)
  (let ((buffer (current-buffer)))
    (dired-up-directory)
    (unless (equal buffer (current-buffer))
      (kill-buffer buffer))))

(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Modified for my needs."
  (interactive)
  (let (sort-by arg)
    (setq sort-by (completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal sort-by "name") (setq arg "-Al"))
     ((equal sort-by "date") (setq arg "-Al -t"))
     ((equal sort-by "size") (setq arg "-Al -S"))
     ((equal sort-by "dir") (setq arg "-Al --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other arg)))

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
  :config
  (setq
   project-list-file (concat camp-var-dir "project-list.el")
   project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project" ".jj")
   project-ignores '("/run/")))

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
                "/run/"
                ".cache/")
               (* any)
               (? (or "html" "pdf" "tex" "epub" "gz")))
          ,(rx "/"
               (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo")
               (* any))))

  (recentf-mode 1))

(provide 'camp-builtin)
