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

(use-package graphql-mode
  :straight t
  :defer t)

(use-package vimrc-mode
  :straight t
  :defer t)

(use-package nushell-mode
  :straight t
  :defer t)

(use-package ledger-mode
  :straight t
  :defer t)

(use-package flycheck-hledger
  :after (flycheck ledger-mode)
  :demand t)

(use-package typst-ts-mode
  :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode"
                 :files (:defaults "*.el")))

(provide 'camp-data)
