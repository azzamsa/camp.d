;; -*- lexical-binding: t; -*-

(use-package dirvish
  :straight t
  :after dired camp-loaded
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$"))
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("p" "~/project/" "Project")
     ("t" "~/.tmp/" "Temp")))
  (dirvish-attributes '(subtree-state nerd-icons file-size vc-state git-msg))
  (dirvish-cache-dir (expand-file-name "dirvish" camp-cache-dir))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t) ; 'global make header line span all panes
  (dirvish-use-mode-line t)
  (dired-omit-mode t)
  :init
  (+map!
    ;; Open
    "o-" '(dirvish :wk "File Manager"))
  :config
  (+nvmap! :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit
    "s" #'dirvish-subtree-toggle
    ;; Can't assign to reserved key. Such as `m`
    "a" #'dired-up-directory
    "y" #'dirvish-yank-menu))

(provide 'camp-files)
