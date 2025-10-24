;; -*- lexical-binding: t; -*-

;; It's Magit! A Git Porcelain inside Emacs.
(use-package magit
  :ensure t
  :after nerd-icons
  :config
  ;; Enable granular diff-highlights for all hunks
  ;; By default, changes are highlighted linewise for all but the selected hunk. This has performance reasons.
  ;; https://magit.vc/manual/magit/Performance.html
  (setq magit-diff-refine-hunk 'all)

  ;; Only enable this to debug your productivity calendar.
  ;; Relative time is hard to pin point.
  ;; (setq magit-log-margin '(t "%Y-%b-%d %I:%M %p " magit-log-margin-width t 18))

  ;; Default args
  (setq transient-values '((magit-rebase "--autosquash" "--autostash" "--committer-date-is-author-date")))

  (setq magit-format-file-function #'magit-format-file-nerd-icons)

  (setq magit-revision-show-gravatars t
        magit-save-repository-buffers nil
        ;; Show in new window
        magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

;; Edit Git commit messages - part of `magit'
(use-package git-commit
  :after magit
  :commands (global-git-commit-mode)
  :hook (git-commit-setup . +git-insert-commit-prefix)
  :custom
  (git-commit-summary-max-length 72) ; defaults to Github's max commit message length
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :init
  (global-git-commit-mode 1))

;; Show source files' TODOs (and FIXMEs, etc) in Magit status buffer
(use-package magit-todos
  :ensure t
  :after magit
  :demand t
  :config
  (magit-todos-mode 1))

;; Emacs package for highlighting uncommitted changes
(use-package diff-hl
  :ensure t
  :hook (find-file    . diff-hl-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (diff-hl-mode . diff-hl-show-hunk-mouse-mode)
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh))

;; Walk through Git revisions of a file
(use-package git-timemachine
  :ensure t
  :config
  (evil-set-initial-state 'git-timemachine-mode 'emacs)
  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details t))

;; Emacs major modes for Git configuration files
(use-package git-modes
  :ensure t
  :mode ("/\\.\\(docker\\|fd\\|rg\\|ag\\|hg\\)?ignore\\'" . gitignore-mode))

(provide 'camp-vc)
