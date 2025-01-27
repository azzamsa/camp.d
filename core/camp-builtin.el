;; -*- lexical-binding: t; -*-

(use-package transient
  :ensure t
  :config
  ;; Map ESC and q to quit transient
  (keymap-set transient-map "<escape>" 'transient-quit-one)
  (keymap-set transient-map "q" 'transient-quit-one)

  (setq transient-history-file (expand-file-name "transient/history.el" camp-var-dir)
        transient-levels-file (expand-file-name "transient/levels.el" camp-var-dir)
        transient-values-file (expand-file-name "transient/values.el" camp-var-dir)))

(use-package password-cache
  :ensure nil
  :custom
  (password-cache t) ; Enable password caching
  (password-cache-expiry 60)) ; One minute, default is 16

(use-package auth-source
  :ensure nil
  :custom
  (auth-sources '("~/.local/share/.authinfo.gpg")) ; Default auth-sources to GPG
  (auth-source-do-cache t) ; Enable caching, do not keep asking about GPG key
  (auth-source-cache-expiry 86400)) ; All day, default is 2h (7200)

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config

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
  :ensure nil
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
  :ensure nil
  :init
  (setq tramp-default-method "ssh")
  :config
  (setopt tramp-auto-save-directory (concat camp-var-dir "tramp/auto-save/")
          tramp-backup-directory-alist backup-directory-alist
          tramp-persistency-file-name (concat camp-var-dir "tramp/persistency.el"))
  :custom
  (tramp-default-remote-shell "/bin/bash"))

(use-package abbrev
  :ensure nil
  :init
  (setq-default abbrev-mode t)
  :config
  (setq abbrev-file-name (concat camp-etc-dir "abbrev.el"))
  (setq save-abbrevs 'silently))

;; Use built-in `treesit' when available
(use-package treesit
  :ensure nil
  :config
  ;; Tree-Sitter grammars
  (add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter" camp-var-dir))
  :custom
  (treesit-font-lock-level 4))

(use-package project
  :ensure nil
  :demand t
  :config
  (setq
   project-list-file (concat camp-var-dir "project-list.el")
   project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project" ".jj")
   project-vc-ignores '("/run/")

   project-switch-commands
   '((?f "Find file" project-find-file)
     (?/ "grep" consult-ripgrep)
     (?d "Dired" project-dired)
     (?g "Magit" magit-project-status)
     (?\e "Escape" keyboard-escape-quit))))

(use-package tab-bar
  :ensure nil
  :custom
  ;; Do not show tabs (`tab-bar' is configured in `camp-workspaces')
  (tab-bar-show nil))

(use-package elisp-mode
  ;; defined in lisp/progmodes/elisp-mode.el
  ;; using `use-package emacs-lisp-mode' produces
  ;; so many oddities
  :ensure nil
  :after evil evil-collection
  :hook (prog-mode-defaults . emacs-lisp-mode)
  :config
  (evil-collection-define-key 'normal 'emacs-lisp-mode-map
    "gz" 'nil))

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package recentf
  :ensure nil
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
               (or "/\\.emacs\\.d/var/" "eln-cache" "/cache/"
                   "/run/" "/var/" ".cache/")
               (* any)
               (? (or "html" "pdf" "tex" "epub" "gz")))
          ,(rx "/"
               (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo")
               (* any))))

  (recentf-mode 1))

(use-package savehist
  :ensure nil
  :config
  (setq savehist-autosave-interval 60     ; save on kill only
        savehist-save-minibuffer-history t
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring vertico-repeat-history) ; persist searches
        savehist-file (expand-file-name "savehist" camp-var-dir))

  (savehist-mode +1))


;; Remember last cursor position in a file
(use-package saveplace
  :ensure nil
  :config
  (setq save-place-file (expand-file-name "saveplace" camp-var-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package flymake
  :ensure nil)

(use-package whitespace
  :ensure nil
  :config
  ;; Show trailing whitespace in `prog-mode' and `conf-mode'
  (defun +show-trailing-whitespace-h ()
    (setq-local show-trailing-whitespace t))
  (add-hook 'prog-mode-hook  #'+show-trailing-whitespace-h)
  (add-hook 'conf-mode-hook  #'+show-trailing-whitespace-h)
  (add-hook 'text-mode-hook  #'+show-trailing-whitespace-h)

  ;; Cleanup is handled by ws-butler
  ;; (add-hook 'before-save-hook #'whitespace-cleanup)

  ;; limit line length
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing lines-tail space-before-tab)))

(provide 'camp-builtin)
