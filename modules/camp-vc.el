;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :after transient
  :init
  (+map! :infix "g"
    "g" #'magit-status)
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
  :custom
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  ;; Show in new window
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

(use-package magit-todos
  :ensure t
  :after magit
  :demand t
  :config
  (magit-todos-mode 1))

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

(use-package git-timemachine
  :ensure t
  :config
  (evil-set-initial-state 'git-timemachine-mode 'emacs)
  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details t))

;; File icons for Magit based on `nerd-icons'
(use-package magit-file-icons
  :ensure t
  :after magit
  :init
  (magit-file-icons-mode 1))


(provide 'camp-vc)
