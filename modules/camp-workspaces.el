;; -*- lexical-binding: t; -*-

(use-package tabspaces
  :straight t
  :after camp-loaded
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Main")
  :init
  (+map! :infix "TAB"
    "TAB" '(tabspaces-switch-or-create-workspace :w "Switch or create")
    "o" '(tabspaces-open-or-create-project-and-workspace :wk "Open or create project")
    "f" '(tabspaces-project-switch-project-open-file :wk "Switch project & open file")
    "d" #'tabspaces-close-workspace
    "b" #'tabspaces-switch-to-buffer
    "t" #'tabspaces-switch-buffer-and-tab
    "C" #'tabspaces-clear-buffers
    "r" #'tabspaces-remove-current-buffer
    "R" #'tabspaces-remove-selected-buffer
    "k" #'(tabspaces-kill-buffers-close-workspace :wk "Kill buffers & close WS")))

(provide 'camp-workspaces)
