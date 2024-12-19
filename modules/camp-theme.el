;; -*- lexical-binding: t; -*-

(use-package doom-themes
  :ensure t
  :config
  (setq doom-tokyo-night-brighter-comments t)
  (setq doom-tokyo-night-comment-bg nil)
  (load-theme 'doom-tokyo-night t)

  (with-eval-after-load 'doom-themes
    (custom-set-faces
     `(default ((t (:foreground "#eceff4" :background "#000000"))))
     `(region ((t (:background "#2d3f76"))))
     `(selection ((t (:background "#2d3f76"))))
     `(neo-file-link-face ((t (:foreground "#eceff4"))))
     `(magit-diff-file-heading ((t (:foreground "#eceff4")))))))

(use-package catppuccin-theme
  :disabled
  ;; Credit to @nullchilly for coming up with this black variant.
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)

  ;; `Overlay0` is too dim AA (3.35:1). `Overlay2` on base is (5.81:1).
  ;; but `Overlay2` is too bright for a comment.
  ;; Using `Subtext0` will get AAA, but it too similar to `Text`.
  (catppuccin-set-color 'overlay0 "#7f849c" 'mocha)

  ;; Catppuccin black.
  (catppuccin-set-color 'base "#000000" 'mocha)
  (catppuccin-set-color 'mantle "#000000" 'mocha)
  (catppuccin-set-color 'crust "#000000" 'mocha)

  (catppuccin-reload)
  (load-theme 'catppuccin t)

  (with-eval-after-load 'catppuccin-theme
    ;; Must be used *after* the theme is loaded
    (custom-set-faces
     ;; The default `region` color for Mocha looks unpleasant on a black background.
     `(region ((t (:background "#313244"))))
     ;; The default `highlight` color for Mocha is invisible on a black background.
     `(highlight ((t (:background "#313244")))))))
