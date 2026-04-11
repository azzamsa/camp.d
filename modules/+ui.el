;; -*- lexical-binding: t; -*-

;;
;; Icons

;; Nerd Font icons for Emacs
(use-package nerd-icons
  :ensure t)

;; A fancy and fast mode-line inspired by minimalism design
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-bar-width 5
        doom-modeline-height 37
        doom-modeline-buffer-encoding nil)
  (setq doom-modeline-check-simple-format t  ; lighter checker segment
        doom-modeline-env-version nil         ; skip runtime version (python/ruby/etc)
        doom-modeline-workspace-name nil)
  (doom-modeline-mode 1))

;; Display typographical ligatures in major modes
(use-package ligature
  :ensure t
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures 'prog-mode
                          '(;; Common programming
                            "==" "!=" ">=" "<=" "::" ":::" ".." "..." "..="
                            "+=" "-=" "*=" "/=" "&&" "||" "--" "++" "__"
                            "|>" "<|" "<|>" "#!" "#[" "]#"
                            ;; Arrows
                            "->" "<-" "=>" "<=>" "<->" "-->" "<--" "->>" "<<-"
                            ;; Rust-specific
                            "/*" "*/" "//" "///" "?." "??" "|=" "^="
                            ;; Typography
                            "ff" "fi" "fl" "ffi")))

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-width nil)
  (visual-fill-column-center-text t))

(use-package minimap
  :ensure t
  :defer t
  :config
  (setq minimap-window-location 'right
        minimap-update-delay 0
        minimap-width-fraction 0.09
        minimap-minimum-width 5))

(use-package dashboard
  :ensure t
  :demand t
  :init
  (defun daily-quote (_list-size)
    (insert (propertize "🦾 Practice, It's Practice, Practice." 'face 'bold)))
  :custom
  (dashboard-center-content t)
  (dashboard-set-heading-iconQ t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-banner-logo-title "Want to go camping?")
  (dashboard-startup-banner (concat user-emacs-directory "docs/logo.png"))
  (dashboard-image-banner-max-width 600)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((daily-quote)
                     (recents . 5)
                     (projects . 5)))
  (dashboard-item-generators '((daily-quote . daily-quote)
                               (recents . dashboard-insert-recents)
                               (projects . dashboard-insert-projects)))
  :config
  (dashboard-setup-startup-hook))

(provide '+ui)
