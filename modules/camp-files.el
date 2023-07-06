;; -*- lexical-binding: t; -*-

(use-package dirvish
  :straight t
  :after dired camp-loaded
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("p" "~/project/" "Project")
     ("t" "~/.tmp/" "Temp")))
  (dirvish-attributes '(subtree-state nerd-icons file-size vc-state git-msg))
  (dirvish-cache-dir (expand-file-name "dirvish" camp-cache-dir))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t) ; 'global make header line span all panes
  (dirvish-use-mode-line t)
  (dired-omit-mode t)

  ;; dirvish-side
  (dirvish-side-width 30)
  (dirvish-side-follow-mode)
  (dirvish-side-auto-expand t)

  :init
  (+map!
    ;; Open
    "o-" '(dirvish :wk "File Manager"))
  :config
  (+nvmap! :keymaps 'dirvish-mode-map
    "?"          '(dirvish-dispatch                   :wk "Dispatch")
    "a"          '(dired-up-directory                 :wk "Up directory")
    "f"          '(dirvish-file-info-menu             :wk "File info menu")
    "q"          '(dirvish-quit                       :wk "Quit")
    "s"          '(dirvish-subtree-toggle             :wk "Toggle subtree")
    "TAB"        '(dirvish-subtree-toggle             :wk "Toggle subtree")
    "X"          '(dired-do-flagged-delete            :wk "Flagged delete")
    "x"          '(dired-do-delete                    :wk "Delete")
    "y"          '(dirvish-yank-menu                  :wk "Yank menu")
    [remap dired-sort-toggle-or-edit]   '(dirvish-quicksort         :wk "Toggle or edit sort order")
    [remap dired-do-redisplay]          '(dirvish-ls-switches-menu  :wk "LS switches menu")
    ;; I need to simple copy file in place with different name
    ;; [remap dired-do-copy]               '(dirvish-yank-menu         :wk "Copy menu")
    )

  (+map-local! :keymaps 'dirvish-mode-map
    "h"          '(dired-omit-mode             :wk "Omit uninteresting files")))

(provide 'camp-files)
