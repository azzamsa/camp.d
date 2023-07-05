;; -*- lexical-binding: t; -*-

(use-package magit
  :straight t
  :after camp-loaded
  :init
  (+map! :infix "g"
    "g" #'magit-status)
  :config
  ;; Most of the Magit keybindings don't work seamlessly in visual mode.
  ;; Invoking 'evil-insert' is needed every time in the 'magit-status-mode'.
  (evil-set-initial-state 'magit-status-mode 'emacs)
  ;; Avoid invoking `evil-insert` in `commit-mode`
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (add-hook 'git-rebase-mode-hook 'evil-insert-state)
  ;; Show all differences
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
    "C-p" #'git-timemachine-show-previous-revision
    "C-n" #'git-timemachine-show-next-revision
    "gb"  #'git-timemachine-blame
    "gtc" #'git-timemachine-show-commit)
  :config
  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details t))


(provide 'camp-vc)
