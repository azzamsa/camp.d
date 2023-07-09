;; -*- lexical-binding: t; -*-

(use-package magit
  :straight t
  :after camp-loaded
  :init
  (+map! :infix "g"
    "g" #'magit-status)

  (+map-local! :keymaps 'magit-mode-map
    "l" '(magit-log-current :wk "Show log")
    ;; `evil-insert` can'b be invoked with `u` in `magit-status` buffer.
    ;; `M-x evil-insert` is uncompfortable to type.
    "u" '(evil-insert-state :wk "Evil insert"))

  (+nvmap! :keymaps 'git-rebase-mode-map
    "M-e"  #'git-rebase-move-line-up
    "M-n"  #'git-rebase-move-line-down)
  :config
  ;; Avoid invoking `evil-insert` everytime.
  (evil-set-initial-state 'magit-status-mode 'insert)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'git-rebase-mode 'insert)

  ;; I love to have my backticks highlighted
  (setq git-commit-major-mode 'markdown-mode)

  ;; Enable granular diff-highlights for all hunks
  ;; By default, changes are highlighted linewise for all but the selected hunk. This has performance reasons.
  ;; https://magit.vc/manual/magit/Performance.html
  (setq magit-diff-refine-hunk 'all)

  ;; Relative time is hard to pin point.
  (setq magit-log-margin '(t "%Y-%b-%d %I:%M %p " magit-log-margin-width t 18))
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  ;; Show in new window
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

(use-package magit-todos
  :straight t
  :after magit
  :demand t
  :config
  (magit-todos-mode 1))

(use-package diff-hl
  :straight t
  :hook (find-file    . diff-hl-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (diff-hl-mode . diff-hl-show-hunk-mouse-mode)
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh))

(use-package git-timemachine
  :straight t
  :after camp-loaded
  :init
  (+nvmap! :keymaps 'git-timemachine-mode-map
    "C-n" #'git-timemachine-show-previous-revision
    "C-e" #'git-timemachine-show-next-revision
    "gb"  #'git-timemachine-blame
    "gtc" #'git-timemachine-show-commit)
  :config
  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details t))


(provide 'camp-vc)
