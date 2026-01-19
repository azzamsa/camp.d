;; -*- lexical-binding: t; -*-

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  (setq modus-themes-common-palette-overrides
        '(
          (bg-main     "#000000")
          (bg-active   "#1d2235") ; bg-dim
          (bg-dim      "#0d0e1c") ; bg-main
          (fg-dim      "#989898")

          ;; All the colors below are kept across theme.
          (bg-main   bg-main)
          ;; Modus Vivendi Tinted uses `red-faint` instead of `fg-dim` as a comment.
          (comment fg-dim)
          (bg-region bg-active)

          ;; Modeline
          (bg-mode-line-active bg-active)
          (border-mode-line-active bg-mode-line-active)
          (bg-mode-line-inactive bg-dim)
          (border-mode-line-inactive bg-mode-line-inactive)

          ;; Line numbers
          (bg-line-number-active bg-dim)))

  (load-theme 'modus-vivendi-tinted t))
