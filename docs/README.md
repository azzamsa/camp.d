# Documentation

## Setting up other theme

Doom themes.

```lisp
(use-package doom-themes
  :straight t
  :config
  ;; (load-theme 'doom-dracula t)
  (load-theme 'doom-tokyo-night t))
```

Ef themes.

```lisp
(use-package ef-themes
  :straight t
  :config
  (load-theme 'ef-night :no-confirm))
```

Modus themes.

```lisp
(use-package modus-themes
  :straight t
  :config
  (load-theme 'modus-vivendi :no-confirm))
```

Catppuccin theme.

```lisp
(use-package catppuccin-theme
  :straight (catppuccin-theme :type git :flavor melpa :host github :repo "catppuccin/emacs")
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))
```
