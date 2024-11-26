# Documentation

## Setting up other theme

```lisp
(use-package doom-themes
  :straight t
  :config
  ;; (load-theme 'doom-dracula t)
  ;; (load-theme 'doom-tokyo-night t))

(use-package catppuccin-theme
  :straight (catppuccin-theme :type git :flavor melpa :host github :repo "catppuccin/emacs")
  :config

  ;; During the day
  ;; (setq catppuccin-flavor 'latte)

  ;; During the night
  (setq catppuccin-flavor 'mocha)

  ;; Mocha colors with macchiato background make it more readable
  (catppuccin-set-color 'base "#24273a" 'mocha)
  (catppuccin-set-color 'surface0 "#363a4f" 'mocha) ;  macchiato: surface0

  (catppuccin-reload)
  (load-theme 'catppuccin :no-confirm))
```

## Useful links

- [Doom's default keybindings](https://github.com/doomemacs/doomemacs/blob/a89d4b7df556bb8b309d1c23e0b60404e750f156/modules/config/default/%2Bevil-bindings.el#L278)
