;; -*- lexical-binding: t; -*-

(use-package modus-themes
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
          (text       "#cdd6f4")
          (subtext1   "#bac2de")
          (subtext0   "#a6adc8")
          (overlay2   "#9399b2")
          (overlay0   "#7f849c")
          (surface0   "#313244")
          (base       "#1e1e2e")
          (crust      "#11111b")

          ;; All the colors below are kept across theme.
          ;; Mostly taken from https://catppuccin.com/palette mocha.
          (bg-main   black)
          ;; Modus Vivendi Tinted uses `red-faint` instead of `fg-dim` as a comment.
          (comment subtext0)
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
