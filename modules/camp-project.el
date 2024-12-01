;; -*- lexical-binding: t; -*-

(use-package consult-project-extra :ensure t)

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

(provide 'camp-project)
