;; -*- lexical-binding: t; -*-

(use-package doom-themes
  ;; :disabled
  ;; I need a fork because many `custom-set-faces` needed if I change the background color.
  ;; This is because many of the colors are inherited from the `bg` color.
  :ensure (doom-themes :repo ("~/projects/scratchpad/forks/doom-themes/" . "doom-themes") :branch "mine")
  :config
  (setq doom-dracula-brighter-comments t)
  (setq doom-dracula-comment-bg nil)
  ;; (load-theme 'doom-tokyo-night-moon t))
  ;; (load-theme 'doom-laserwave-high-contrast t))
  (load-theme 'doom-dracula t))

(use-package catppuccin-theme
  :disabled
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)

  ;; `Overlay0` is too dim AA (3.35:1). `Overlay2` on base is (5.81:1).
  ;; but `Overlay2` is too bright for a comment.
  ;; Using `Subtext0` will get AAA (9.43:1), but it too similar to `Text`.
  ;; (catppuccin-set-color 'overlay0 "#7f849c" 'mocha)
  ;; I think I will try Latte's overlay0 (8.06:1)
  ;; Modus Vivendi uses `#989898` (7.27:1)
  (catppuccin-set-color 'overlay0 "#9CA0B0" 'mocha)

  ;; Catppuccin black.
  (catppuccin-set-color 'base "#000000" 'mocha)
  (catppuccin-set-color 'mantle "#000000" 'mocha)
  (catppuccin-set-color 'crust "#000000" 'mocha)
  (catppuccin-set-color 'text "#ffffff" 'mocha)

  (load-theme 'catppuccin t)

  (with-eval-after-load 'catppuccin-theme
    ;; Must be used *after* the theme is loaded
    (custom-set-faces
     ;; The default `region` and `highlight` colors in Mocha don't look good against a black background.
     `(region ((t (:background "#313244"))))
     `(highlight ((t (:background "#313244")))))))

(use-package modus-themes
  ;; :disabled
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  ;; Modeline & Line-number will be the same across themes.
  ;; Other faces will be leave as is.
  (setq modus-themes-common-palette-overrides
        '(
          (black      "#000000")
          ;; Catppuccin Mocha palette
          (crust      "#11111b")
          (base       "#1e1e2e")
          (surface0   "#313244")
          (overlay0   "#7f849c")
          (overlay2   "#9399b2")
          (text       "#cdd6f4")

          ;; All the colors below are kept across theme.
          ;; Mostly taken from https://catppuccin.com/palette mocha.
          (bg-main   black)
          ;; Modus Vivendi Tinted uses `red-faint` instead of `fg-dim` as a comment.
          (comment fg-dim)
          (bg-region surface0)

          ;; Modeline
          ;; (fg-mode-line-active text)
          (bg-mode-line-active base)
          (border-mode-line-active bg-mode-line-active)
          ;; (fg-mode-line-inactive overlay2)
          (bg-mode-line-inactive crust)
          (border-mode-line-inactive bg-mode-line-inactive)

          ;; Line numbers
          ;; (fg-line-number-active text)
          (bg-line-number-active surface0)
          ;; (fg-line-number-inactive overlay2)
          (bg-line-number-inactive crust)

          ;; Fringe
          (fringe crust)))

  (load-theme 'modus-vivendi-tinted t))
