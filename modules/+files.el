;; -*- lexical-binding: t; -*-

;; Collection of useful dired additions
(use-package dired-hacks
  :ensure (:host github :repo "Fuco1/dired-hacks"))

(use-package dirvish
  :ensure t
  :after dired
  :init
  (setq dirvish-cache-dir (expand-file-name "dirvish" camp-cache-dir))
  :config
  (+nvmap! :keymaps 'dirvish-mode-map
    "?"      '(dirvish-dispatch             :wk "Dispatch menu")
    "q"      '(dirvish-quit                 :wk "Quit Dirvish")
    "b"      '(dirvish-quick-access         :wk "Quick access")
    "f"      '(dirvish-file-info-menu       :wk "File info")

    "p"      '(dirvish-yank                 :wk "Yank (Dirvish)")
    "z"      '(dirvish-history-jump         :wk "Jump history")

    "h"      '(dired-up-directory           :wk "Go up directory")
    "l"      '(dired-find-file              :wk "Open file")
    [left]   '(dired-up-directory           :wk "Go up directory")
    [right]  '(dired-find-file              :wk "Open file")

    "s"      '(dirvish-subtree-toggle       :wk "Toggle subtree")
    "TAB"    '(dirvish-subtree-toggle       :wk "Toggle subtree")

    "X"      '(dired-do-flagged-delete      :wk "Delete flagged")
    "x"      '(dired-do-delete              :wk "Delete file")

    "yl"     '(dirvish-copy-file-true-path  :wk "Yank real path")
    "yn"     '(dirvish-copy-file-name       :wk "Yank file name")
    "yp"     '(dirvish-copy-file-path       :wk "Yank file path")
    "yy"     '(dired-do-copy                :wk "Copy file"))

  (+map-local! :keymaps 'dirvish-mode-map
    "h" '(dired-omit-mode         :wk "Omit uninteresting files"))
  :config
  (setq dirvish-hide-details t)
  (dirvish-override-dired-mode)
  (setq dirvish-attributes '(nerd-icons subtree-state file-size))
  (setq dirvish-quick-access-entries
        '(("h" "~/"      "Home")
          ("l" "~/labs/" "Labs")
          ("p" "~/code/" "Code")
          ("o" "~/hq/"   "Hq")
          ("t" "/tmp"    "/tmp"))))

(provide '+files)
