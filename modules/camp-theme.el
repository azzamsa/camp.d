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
  (load-theme 'doom-snazzy t))

(use-package catppuccin-theme
  :disabled
  :ensure (catppuccin-theme :repo "~/playground/forks/catppuccin-emacs/" :branch "mine")
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))

(use-package modus-themes
  ;; Moved to `personal` config as I change theme frequently.
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
          (comment overlay0)
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
          (fringe crust)

          ;; Dracula palette
          (red        "#ff5555")
          (orange     "#ffb86c")
          (green      "#50fa7b")
          (teal       "#0189cc")
          (yellow     "#f1fa8c")
          (blue       "#61bfff")
          (dark-blue  "#0189cc")
          (magenta    "#ff79c6")
          (violet     "#bd93f9")
          (cyan       "#8be9fd")

          (accent-1 magenta)
          (fg-prompt accent-1)

          ;; Code
          (builtin orange)
          (constant cyan)
          (fnname green)
          (keyword magenta)
          (string yellow)
          (variable violet)
          (type violet)

          ;; Heading mappings
          (fg-heading-0 accent-1)
          (fg-heading-1 accent-1)
          (fg-heading-2 orange)
          (fg-heading-3 green)
          (fg-heading-4 green)))


  (load-theme 'modus-vivendi-tinted t))

