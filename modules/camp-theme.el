;; -*- lexical-binding: t; -*-

(use-package doom-themes
  :disabled
  ;; I need a fork because many `custom-set-faces` needed if I change the background color.
  ;; This is because many of the colors are inherited from the `bg` color.
  ;;
  ;; :ensure (doom-themes :repo "https://github.com/azzamsa/doom-themes" :branch "my-tokyo-night")
  ;; for debugging purpose
  :ensure (doom-themes :repo "~/playground/forks/doom-themes/" :branch "mine")
  :config
  (setq doom-tokyo-night-brighter-comments t)
  (setq doom-tokyo-night-comment-bg nil)
  ;; (load-theme 'doom-tokyo-night-moon t)

  ;; default `dracula` comment is readable enough. No need `dracula-brighter-comments`.
  (load-theme 'doom-dracula t))

(use-package catppuccin-theme
  :ensure (catppuccin-theme :repo "~/playground/forks/catppuccin-emacs/" :branch "mine")
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))
