;; -*- lexical-binding: t; -*-

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  (setq neo-smart-open t)
  ;; (setq neo-autorefresh t)
  (setq neo-hidden-regexp-list
        '(
          ;; hidden files
          "^\\."
          ;; temp files
          "~$" "^#.*#$"
          ;; generated files
          "^__"
          ;; docker
          "cdata"
          )))

(defun neotree-project-dir ()
  "Always open NeoTree in project root."
  (interactive)
  (let ((project-dir (ignore-errors (project-root (project-current))))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (if file-name
                  (neotree-find file-name))))
      (message "Could not find project root."))))

;; One tab per project, with unique names - simple implementation of workspaces
(use-package otpp
  :ensure (:host github :repo "abougouffa/one-tab-per-project")
  :after project
  :bind (("C-x t D" . otpp-detach-buffer-to-tab)
         ("C-x t C" . otpp-change-tab-root-dir)
         ("C-x t P" . otpp-prefix))
  :custom
  (otpp-project-aware-commands-regexp (rx (seq bol (or "project-" "+project-" "projection-"))))
  :init
  (otpp-mode 1)
  (otpp-override-mode 1))

(provide 'camp-project)
