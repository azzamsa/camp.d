;; -*- lexical-binding: t; -*-

(use-package tokyo-night
  :vc (:url "https://github.com/bbatsov/tokyo-night-emacs" :rev :newest)
  :config
  (setq  tokyo-night-override-colors-alist
         '(
           ;;
           ;; Tokyo black
           ;;

           ;; Background shades
           ("tokyo-bg-darkest"    . "#000000")
           ("tokyo-bg-dark"       . "#000000")
           ("tokyo-bg"            . "#000000")

           ;; Foreground shades
           ("tokyo-fg"            . "#ffffff")
           ("tokyo-fg-dark"       . "#c8d3f5")
           ("tokyo-fg-muted"      . "#828bb8")
           ("tokyo-fg-gutter"     . "#3b4261")

           ;; UI elements
           ("tokyo-line-nr"       . "#828bb8")
           ("tokyo-line-nr-cur"   . "#fca7ea")
           ("tokyo-selection"     . "#292e42") ; 13.42:1 on white

           ;; brighter comments
           ("tokyo-comment" . "#9aa5ce")))
  (load-theme 'tokyo-night-moon t))

(use-package modus-themes
  :disabled
  :ensure t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)
  (setq modus-themes-common-palette-overrides
        '(
          ;; magenta-intense is too harsh
          ;; (cursor magenta-warmer)

          (bg-main     "#000000")
          (bg-dim      "#0d0e1c") ; bg-main
          (bg-active   "#1d2235") ; bg-dim

          (bg-main   bg-main)
          ;; Modeline
          (bg-mode-line-active bg-active)
          (border-mode-line-active bg-mode-line-active)
          (bg-mode-line-inactive bg-dim)
          (border-mode-line-inactive bg-mode-line-inactive)
          ;; Line numbers
          (bg-line-number-active bg-dim)))
  (load-theme 'modus-vivendi t))
