;; -*- lexical-binding: t; -*-

;; Collection of useful dired additions
(use-package dired-hacks
  :ensure (:host github :repo "Fuco1/dired-hacks"))

(use-package dirvish
  :ensure (dirvish :type git :host github :repo "hlissner/dirvish")
  :after dired
  :config
  (setq dirvish-cache-dir (expand-file-name "dirvish" camp-cache-dir))

  (+nvmap! :keymaps 'dirvish-mode-map
    "?"      '(dirvish-dispatch           :wk "Dispatch")
    "q"      '(dirvish-quit               :wk "Quit")
    "b"      '(dirvish-quick-access       :wk "Quick Access")
    "f"      '(dirvish-file-info-menu     :wk "File info menu")
    "p"      '(dirvish-yank               :wk "File info menu")
    "z"      '(dirvish-history-jump       :wk "File info menu")
    "h"      '(dired-up-directory         :wk "File info menu")
    "l"      '(dired-find-file            :wk "File info menu")
    [left]   '(dired-up-directory         :wk "File info menu")
    [right]  '(dired-find-file            :wk "File info menu")
    "s"      '(dirvish-subtree-toggle     :wk "Toggle subtree")
    "TAB"    '(dirvish-subtree-toggle     :wk "Toggle subtree")
    "X"      '(dired-do-flagged-delete    :wk "Flagged delete")
    "x"      '(dired-do-delete            :wk "Delete")
    "yl"     '(dirvish-copy-file-true-path :wk "Copy file true path")
    "yn"     '(dirvish-copy-file-name     :wk "Copy file name")
    "yp"     '(dirvish-copy-file-path     :wk "Copy file path")
    "yy"     '(dired-do-copy              :wk "Yank menu"))

  (+map-local! :keymaps 'dirvish-mode-map
    "h" '(dired-omit-mode         :wk "Omit uninteresting files"))
  :config
  (dirvish-override-dired-mode)

  (setq dirvish-attributes '(nerd-icons subtree-state file-size vc-state git-msg))
  (setq dirvish-quick-access-entries
        '(("h" "~/"            "Home")
          ("l" "~/playground/" "Playgound")
          ("p" "~/projects/"   "Projects")
          ("o" "~/office/"     "Office")
          ("t" "/tmp"          "/tmp")))
  (setq
   dirvish-use-header-line 'global
   dirvish-mode-line-format '(:left (evil-state sort file-time symlink) :right (omit yank index))
   dirvish-use-mode-line t)

  (dirvish-define-mode-line evil-state ()
    (cond
     ((evil-normal-state-p) (propertize "🅝" 'face 'font-lock-string-face))
     ((evil-emacs-state-p) (propertize "🅔" 'face 'font-lock-builtin-face))
     ((evil-insert-state-p) (propertize "🅘" 'face 'font-lock-keyword-face))
     ((evil-visual-state-p) (propertize "🅥" 'face 'font-lock-warning-face))
     (t "🅤"))))

(provide 'camp-files)
