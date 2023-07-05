;; -*- lexical-binding: t; -*-

(use-package yaml-mode
  :straight t
  :mode "Procfile\\'")

(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

(use-package json-mode
  :straight t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :config
  (+map-local! :keymaps 'json-mode-map
    "p" #'json-mode-show-path
    "t" #'json-toggle-boolean
    "d" #'json-mode-kill-path
    "x" #'json-nullify-sexp
    "+" #'json-increment-number-at-point
    "-" #'json-decrement-number-at-point
    "f" #'json-mode-beautify))

(use-package just-mode
  :straight t
  :defer t)

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode))

(provide 'camp-data)
