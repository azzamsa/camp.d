;; -*- lexical-binding: t; -*-

(use-package project
  :straight (:type built-in)
  :after camp-loaded
  :demand t
  :custom
  (project-list-file (concat camp-cache-dir "project-list.el"))
  (project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project")))

(defun +project-from-dir (&optional dir)
  "Return project instance if DIR is a valid project."
  (project--find-in-directory dir))

(use-package consult-project-extra
  :straight t)

(use-package neotree
  :straight t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  ;; (setq neo-autorefresh t)
  (setq neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(?:git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
          ;; generated files, caches or local pkgs
          ;; `cdata': container data
          ;; `target': rust generated directory
          "^\\(?:node_modules\\|eln-cache\\|target\\|cdata\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(?:sync\\|export\\|attach\\)$"
          ;; temp files
          "~$"
          "^#.*#$")))

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
