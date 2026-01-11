;; -*- lexical-binding: t; -*-

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  (setq modus-themes-common-palette-overrides
        '(
          (bg-main     "#000000")
          (fg-dim      "#989898")

          ;; All the colors below are kept across theme.
          (bg-main   bg-main)
          ;; Modus Vivendi Tinted uses `red-faint` instead of `fg-dim` as a comment.
          (comment fg-dim)
          (bg-region bg-dim)

          ;; Modeline
          (bg-mode-line-active bg-dim)
          (border-mode-line-active bg-mode-line-active)

          ;; Line numbers
          (bg-line-number-active bg-dim)))

  (load-theme 'modus-vivendi-tinted t))
