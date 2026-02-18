;; -*- lexical-binding: t; -*-

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)
  (setq modus-themes-common-palette-overrides
        '(
          (bg-main     "#000000")
          (bg-dim      "#0d0e1c") ; bg-main
          (bg-active   "#1d2235") ; bg-dim

          (bg-main   bg-main)
          (bg-region bg-active)
          ;; Modeline
          (bg-mode-line-active bg-active)
          (border-mode-line-active bg-mode-line-active)
          (bg-mode-line-inactive bg-dim)
          (border-mode-line-inactive bg-mode-line-inactive)
          ;; Line numbers
          (bg-line-number-active bg-dim)))
  (load-theme 'modus-vivendi-tinted t))
